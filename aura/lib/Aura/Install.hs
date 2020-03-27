{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

-- |
-- Module    : Aura.Install
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Layer for AUR package installation.
-- Backend for `Aura.Commands.A`.

module Aura.Install
  ( install
  , displayPkgDeps
  ) where

import           Aura.Build (buildPackages, installPkgFiles)
import           Aura.Cache (Cache(..), cacheContents)
import           Aura.Colour
import           Aura.Core
import           Aura.Dependencies (resolveDeps)
import           Aura.Diff (diff)
import           Aura.IO
import           Aura.Languages
import           Aura.Packages.AUR (aurLookup)
import           Aura.Pacman (pacman, pacmanSuccess)
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Pkgbuild.Security
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (fmapEither)
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Generics.Product (HasField'(..), field, super)
import           Data.Semigroup.Foldable (fold1)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import           Language.Bash.Pretty (prettyText)
import           Language.Bash.Syntax (ShellCommand)
import           Lens.Micro (each, (^..))
import           RIO hiding (FilePath)
import           RIO.Directory (setCurrentDirectory)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Path (fromAbsoluteFilePath)

---

-- | High level 'install' command. Handles installing
-- dependencies.
install :: NESet PkgName -> RIO Env ()
install pkgs = do
  ss <- asks settings
  if | not $ switch ss DeleteMakeDeps -> install' pkgs
     | otherwise -> do -- `-a` was used.
         orphansBefore <- liftIO orphans
         install' pkgs
         orphansAfter <- liftIO orphans
         let makeDeps = NES.nonEmptySet (orphansAfter S.\\ orphansBefore)
         traverse_ (\mds -> liftIO (notify ss . removeMakeDepsAfter_1 $ langOf ss) *> removePkgs mds) makeDeps

install' :: NESet PkgName -> RIO Env ()
install' pkgs = do
  rpstry   <- asks repository
  ss       <- asks settings
  unneeded <- bool
              (pure S.empty)
              (S.fromList . catMaybes <$> liftIO (traverseConcurrently Par' isInstalled $ toList pkgs))
              $ shared ss NeededOnly
  let !pkgs' = NES.toSet pkgs
  if | shared ss NeededOnly && unneeded == pkgs' -> liftIO . warn ss . install_2 $ langOf ss
     | otherwise -> do
         let (ignored, notIgnored) = S.partition (`S.member` ignoresOf ss) pkgs'
         installAnyway <- confirmIgnored ignored
         case NES.nonEmptySet $ (notIgnored <> installAnyway) S.\\ unneeded of
           Nothing        -> liftIO . warn ss . install_2 $ langOf ss
           Just toInstall -> do
             traverse_ (report yellow reportUnneededPackages_1) . NEL.nonEmpty
               $ toList unneeded
             (nons, toBuild) <- liftMaybeM (Failure connectionFailure_1) . liftIO
               $ aurLookup (managerOf ss) toInstall
             pkgbuildDiffs toBuild
             traverse_ (report red reportNonPackages_1) . NEL.nonEmpty $ toList nons
             let !explicits = bool (S.map (\b -> b { isExplicit = True }) toBuild) toBuild
                   $ switch ss AsDeps
             case NES.nonEmptySet explicits of
               Nothing       -> throwM $ Failure install_2
               Just toBuild' -> do
                 liftIO $ notify ss (install_5 $ langOf ss) *> hFlush stdout
                 allPkgs <- depsToInstall rpstry toBuild'
                 let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
                 unless (switch ss NoPkgbuildCheck)
                   $ traverse_ (traverse_ analysePkgbuild) buildPkgs
                 reportPkgsToInstall repoPkgs buildPkgs
                 unless (switch ss DryRun) $ do
                   continue <- liftIO $ optionalPrompt ss install_3
                   if | not continue -> throwM $ Failure install_4
                      | otherwise    -> do
                          traverse_ repoInstall $ NEL.nonEmpty repoPkgs
                          let !mbuildPkgs = NEL.nonEmpty buildPkgs
                          traverse_ (liftIO . storePkgbuilds . fold1) mbuildPkgs
                          traverse_ buildAndInstall mbuildPkgs

-- | Determine if a package's PKGBUILD might contain malicious bash code.
analysePkgbuild :: Buildable -> RIO Env ()
analysePkgbuild b = do
  ss <- asks settings
  let f = do
        yes <- liftIO $ optionalPrompt ss security_6
        when yes . throwM $ Failure security_7
  case parsedPB $ b ^. field @"pkgbuild" of
    Nothing -> liftIO (warn ss (security_1 (b ^. field @"name") $ langOf ss)) *> f
    Just l  -> case bannedTerms l of
      []  -> pure ()
      bts -> do
        liftIO $ scold ss (security_5 (b ^. field @"name") $ langOf ss)
        liftIO $ traverse_ (displayBannedTerms ss) bts
        f

displayBannedTerms :: Settings -> (ShellCommand, BannedTerm) -> IO ()
displayBannedTerms ss (stmt, b) = do
  putTextLn . T.pack $ "\n    " <> prettyText stmt <> "\n"
  warn ss $ reportExploit b lang
  where lang = langOf ss

-- | Give anything that was installed as a dependency the /Install Reason/ of
-- "Installed as a dependency for another package".
annotateDeps :: NESet Buildable -> IO ()
annotateDeps bs = unless (null bs') . void . pacmanSuccess
  $ ["-D", "--asdeps"] <> asFlag (bs' ^.. each . field @"name")
  where bs' = filter (not . isExplicit) $ toList bs

-- | Reduce a list of candidate packages to build, such that there is only one
-- instance of each "Package Base". This will ensure that split packages will
-- only be built once each. Precedence is given to packages that actually
-- match the base name (e.g. llvm50 vs llvm50-libs).
uniquePkgBase :: [NESet Buildable] -> [NESet Buildable]
uniquePkgBase bs = mapMaybe (NES.nonEmptySet . S.filter (\b -> (b ^. field @"name") `S.member` goods) . NES.toSet) bs
  where f a b | (a ^. field @"name") == (a ^. field @"base") = a
              | (b ^. field @"name") == (b ^. field @"base") = b
              | otherwise = a
        goods = S.fromList . (^.. each . field @"name") . M.elems . M.fromListWith f $ map (view (field @"base") &&& id) bs'
        bs'   = foldMap toList bs

confirmIgnored :: Set PkgName -> RIO Env (Set PkgName)
confirmIgnored (toList -> ps) = do
  ss <- asks settings
  S.fromList <$> filterM (liftIO . optionalPrompt ss . confirmIgnored_1) ps

depsToInstall :: Repository -> NESet Buildable -> RIO Env (NonEmpty (NESet Package))
depsToInstall repo bs = do
  ss <- asks settings
  traverse (liftIO . packageBuildable ss) (NES.toList bs) >>= resolveDeps repo . NES.fromList

repoInstall :: NonEmpty Prebuilt -> RIO Env ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf . settings)
  liftIO . pacman $ ["-S", "--asdeps"] <> pacOpts <> asFlag (ps ^.. each . field @"name")

buildAndInstall :: NonEmpty (NESet Buildable) -> RIO Env ()
buildAndInstall bss = do
  pth   <- asks (either id id . cachePathOf . commonConfigOf . settings)
  cache <- liftIO $ cacheContents pth
  traverse_ (f cache) bss
  where f (Cache cache) bs = do
          ss <- asks settings
          let (ps, cached) = fmapEither g $ toList bs
              g b = case (b ^. super @SimplePkg) `M.lookup` cache of
                Just pp | not (switch ss ForceBuilding) -> Right pp
                _                                       -> Left b
          built <- traverse (buildPackages . NES.fromList) $ NEL.nonEmpty ps
          traverse_ installPkgFiles $ built <> (NES.fromList <$> NEL.nonEmpty cached)
          liftIO $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies. The result of @-Ad@.
displayPkgDeps :: NESet PkgName -> RIO Env ()
displayPkgDeps ps = do
  rpstry <- asks repository
  ss <- asks settings
  let f = depsToInstall rpstry >=> reportDeps (switch ss LowVerbosity) . partitionPkgs
  (_, goods) <- liftMaybeM (Failure connectionFailure_1) . liftIO $ aurLookup (managerOf ss) ps
  traverse_ f $ NES.nonEmptySet goods
  where reportDeps True  = liftIO . uncurry reportListOfDeps
        reportDeps False = uncurry reportPkgsToInstall

reportPkgsToInstall :: [Prebuilt] -> [NESet Buildable] -> RIO Env ()
reportPkgsToInstall rps bps = do
  let (explicits, ds) = L.partition isExplicit $ foldMap toList bps
  f reportPkgsToInstall_1 rps
  f reportPkgsToInstall_3 ds
  f reportPkgsToInstall_2 explicits
  where
    f m xs = traverse_ (report green m) . NEL.nonEmpty . L.sort $ xs ^.. each . field @"name"

reportListOfDeps :: [Prebuilt] -> [NESet Buildable] -> IO ()
reportListOfDeps rps bps = f rps *> f (foldMap toList bps)
  where f :: HasField' "name" s PkgName => [s] -> IO ()
        f = traverse_ putTextLn . L.sort . (^.. each . field' @"name" . field' @"name")

pkgbuildDiffs :: Set Buildable -> RIO Env ()
pkgbuildDiffs ps = asks settings >>= check
  where
    check :: Settings -> RIO Env ()
    check ss | not $ switch ss DiffPkgbuilds = pure ()
             | otherwise = traverse_ displayDiff ps

    displayDiff :: Buildable -> RIO Env ()
    displayDiff p = do
      ss <- asks settings
      let pn   = p ^. field @"name"
          lang = langOf ss
      isStored <- liftIO $ hasPkgbuildStored pn
      if not isStored
        then liftIO . warn ss $ reportPkgbuildDiffs_1 pn lang
        else liftIO $ do
          setCurrentDirectory "/tmp"
          let new = "/tmp/new.pb"
          writeFileBinary new $ p ^. field @"pkgbuild" . field @"pkgbuild"
          liftIO . warn ss $ reportPkgbuildDiffs_3 pn lang
          diff ss (pkgbuildPath pn) $ fromAbsoluteFilePath new
