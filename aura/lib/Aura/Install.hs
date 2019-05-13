{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module    : Aura.Install
-- Copyright : (c) Colin Woodbury, 2012 - 2019
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
import           Aura.Languages
import           Aura.Packages.AUR (aurLookup)
import           Aura.Pacman (pacman, pacmanSuccess)
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Pkgbuild.Security
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (optionalPrompt)
import           BasePrelude hiding (FilePath, diff)
import           Control.Compactable (fmapEither)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Scheduler (Comp(..), traverseConcurrently)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (HasField'(..), field, super)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Semigroup.Foldable (fold1)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text.IO as T
import           Language.Bash.Pretty (prettyText)
import           Language.Bash.Syntax (ShellCommand)
import           Lens.Micro (each, (^.), (^..))
import           Lens.Micro.Extras (view)
import           System.Directory (setCurrentDirectory)
import           System.IO (hFlush, stdout)
import           System.Path (fromAbsoluteFilePath)

---

-- | High level 'install' command. Handles installing
-- dependencies.
install :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
install pkgs = do
  ss <- asks settings
  if | not $ switch ss DeleteMakeDeps -> install' pkgs
     | otherwise -> do -- `-a` was used.
         orphansBefore <- send orphans
         install' pkgs
         orphansAfter <- send orphans
         let makeDeps = NES.fromSet (orphansAfter S.\\ orphansBefore)
         traverse_ (\mds -> send (notify ss . removeMakeDepsAfter_1 $ langOf ss) *> removePkgs mds) makeDeps

install' :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
install' pkgs = do
  rpstry   <- asks repository
  ss       <- asks settings
  unneeded <- bool
              (pure S.empty)
              (S.fromList . catMaybes <$> send (traverseConcurrently Par' isInstalled $ toList pkgs))
              $ shared ss NeededOnly
  let !pkgs' = NES.toSet pkgs
  if | shared ss NeededOnly && unneeded == pkgs' -> send . warn ss . install_2 $ langOf ss
     | otherwise -> do
         let (ignored, notIgnored) = S.partition (`S.member` ignoresOf ss) pkgs'
         installAnyway <- confirmIgnored ignored
         case NES.fromSet $ (notIgnored <> installAnyway) S.\\ unneeded of
           Nothing        -> send . warn ss . install_2 $ langOf ss
           Just toInstall -> do
             traverse_ (report yellow reportUnneededPackages_1) . NEL.nonEmpty $ toList unneeded
             (nons, toBuild) <- liftMaybeM (Failure connectionFailure_1) $ aurLookup (managerOf ss) toInstall
             pkgbuildDiffs toBuild
             traverse_ (report red reportNonPackages_1) . NEL.nonEmpty $ toList nons
             case NES.fromSet $ S.map (\b -> b { isExplicit = True }) toBuild of
               Nothing       -> throwError $ Failure install_2
               Just toBuild' -> do
                 send $ notify ss (install_5 $ langOf ss) *> hFlush stdout
                 allPkgs <- depsToInstall rpstry toBuild'
                 let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
                 unless (switch ss NoPkgbuildCheck) $ traverse_ (traverse_ analysePkgbuild) buildPkgs
                 reportPkgsToInstall repoPkgs buildPkgs
                 unless (switch ss DryRun) $ do
                   continue <- send $ optionalPrompt ss install_3
                   if | not continue -> throwError $ Failure install_4
                      | otherwise    -> do
                          traverse_ repoInstall $ NEL.nonEmpty repoPkgs
                          let !mbuildPkgs = NEL.nonEmpty buildPkgs
                          traverse_ (send . storePkgbuilds . fold1) mbuildPkgs
                          traverse_ buildAndInstall mbuildPkgs

-- | Determine if a package's PKGBUILD might contain malicious bash code.
analysePkgbuild :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) => Buildable -> Eff r ()
analysePkgbuild b = do
  ss <- asks settings
  let f = do
        yes <- send $ optionalPrompt ss security_6
        when yes . throwError $ Failure security_7
  case parsedPB $ b ^. field @"pkgbuild" of
    Nothing -> send (warn ss (security_1 (b ^. field @"name") $ langOf ss)) *> f
    Just l  -> case bannedTerms l of
      []  -> pure ()
      bts -> do
        send $ scold ss (security_5 (b ^. field @"name") $ langOf ss)
        send $ traverse_ (displayBannedTerms ss) bts
        f

displayBannedTerms :: Settings -> (ShellCommand, BannedTerm) -> IO ()
displayBannedTerms ss (stmt, b) = do
  putStrLn $ "\n    " <> prettyText stmt <> "\n"
  warn ss $ reportExploit b lang
  where lang = langOf ss

-- | Give anything that was installed as a dependency the /Install Reason/ of
-- "Installed as a dependency for another package".
annotateDeps :: NonEmptySet Buildable -> IO ()
annotateDeps bs = unless (null bs') . void . pacmanSuccess $ ["-D", "--asdeps"] <> asFlag (bs' ^.. each . field @"name")
  where bs' = filter (not . isExplicit) $ toList bs

-- | Reduce a list of candidate packages to build, such that there is only one
-- instance of each "Package Base". This will ensure that split packages will
-- only be built once each. Precedence is given to packages that actually
-- match the base name (e.g. llvm50 vs llvm50-libs).
uniquePkgBase :: [NonEmptySet Buildable] -> [NonEmptySet Buildable]
uniquePkgBase bs = mapMaybe (NES.fromSet . S.filter (\b -> (b ^. field @"name") `S.member` goods) . NES.toSet) bs
  where f a b | (a ^. field @"name") == (a ^. field @"base") = a
              | (b ^. field @"name") == (b ^. field @"base") = b
              | otherwise = a
        goods = S.fromList . (^.. each . field @"name") . M.elems . M.fromListWith f $ map (view (field @"base") &&& id) bs'
        bs'   = foldMap toList bs

confirmIgnored :: (Member (Reader Env) r, Member IO r) => S.Set PkgName -> Eff r (S.Set PkgName)
confirmIgnored (toList -> ps) = do
  ss <- asks settings
  S.fromList <$> filterM (send . optionalPrompt ss . confirmIgnored_1) ps

depsToInstall :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Buildable -> Eff r (NonEmpty (NonEmptySet Package))
depsToInstall repo bs = do
  ss <- asks settings
  traverse (send . packageBuildable ss) (NES.toNonEmpty bs) >>= resolveDeps repo . NES.fromNonEmpty

repoInstall :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) => NonEmpty Prebuilt -> Eff r ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf . settings)
  liftEitherM . pacman $ ["-S", "--asdeps"] <> pacOpts <> asFlag (ps ^.. each . field @"name")

buildAndInstall :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  NonEmpty (NonEmptySet Buildable) -> Eff r ()
buildAndInstall bss = do
  pth   <- asks (either id id . cachePathOf . commonConfigOf . settings)
  cache <- send $ cacheContents pth
  traverse_ (f cache) bss
  where f (Cache cache) bs = do
          ss <- asks settings
          let (ps, cached) = fmapEither g $ toList bs
              g b = case (b ^. super @SimplePkg) `M.lookup` cache of
                Just pp | not (switch ss ForceBuilding) -> Right pp
                _                                       -> Left b
          built <- traverse (buildPackages . NES.fromNonEmpty) $ NEL.nonEmpty ps
          traverse_ installPkgFiles $ built <> (NES.fromNonEmpty <$> NEL.nonEmpty cached)
          send $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies. The result of @-Ad@.
displayPkgDeps :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
displayPkgDeps ps = do
  rpstry <- asks repository
  ss <- asks settings
  let f = depsToInstall rpstry >=> reportDeps (switch ss LowVerbosity) . partitionPkgs
  (_, goods) <- liftMaybeM (Failure connectionFailure_1) $ aurLookup (managerOf ss) ps
  traverse_ f $ NES.fromSet goods
  where reportDeps True  = send . uncurry reportListOfDeps
        reportDeps False = uncurry reportPkgsToInstall

reportPkgsToInstall :: (Member (Reader Env) r, Member IO r) =>
   [Prebuilt] -> [NonEmptySet Buildable] -> Eff r ()
reportPkgsToInstall rps bps = do
  let (explicits, ds) = partition isExplicit $ foldMap toList bps
  f reportPkgsToInstall_1 rps
  f reportPkgsToInstall_3 ds
  f reportPkgsToInstall_2 explicits
  where f m xs = traverse_ (report green m) . NEL.nonEmpty . sort $ xs ^.. each . field @"name"

reportListOfDeps :: [Prebuilt] -> [NonEmptySet Buildable] -> IO ()
reportListOfDeps rps bps = f rps *> f (foldMap toList bps)
  where f :: HasField' "name" s PkgName => [s] -> IO ()
        f = traverse_ T.putStrLn . sort . (^.. each . field' @"name" . field' @"name")

pkgbuildDiffs :: (Member (Reader Env) r, Member IO r) => S.Set Buildable -> Eff r ()
pkgbuildDiffs ps = asks settings >>= check
    where check ss | not $ switch ss DiffPkgbuilds = pure ()
                   | otherwise = traverse_ displayDiff ps
          displayDiff :: (Member (Reader Env) r, Member IO r) => Buildable -> Eff r ()
          displayDiff p = do
            ss <- asks settings
            let pn   = p ^. field @"name"
                lang = langOf ss
            isStored <- send $ hasPkgbuildStored pn
            if not isStored
               then send . warn ss $ reportPkgbuildDiffs_1 pn lang
               else send $ do
                 setCurrentDirectory "/tmp"
                 let new = "/tmp/new.pb"
                 BL.writeFile new $ p ^. field @"pkgbuild" . field @"pkgbuild"
                 liftIO . warn ss $ reportPkgbuildDiffs_3 pn lang
                 diff ss (pkgbuildPath pn) $ fromAbsoluteFilePath new
