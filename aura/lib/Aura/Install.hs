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
import           Aura.Packages.AUR (aurLookup, aurRepo)
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman (pacman, pacmanSuccess)
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Pkgbuild.Security
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (optionalPrompt)
import           BasePrelude hiding (FilePath, diff)
import           Control.Compactable (fmapEither)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Throttled (throttle)
import           Control.Effect (Carrier, Member)
import           Control.Effect.Error (Error, throwError)
import           Control.Effect.Lift (Lift, sendM)
import           Control.Effect.Reader (Reader, ask, asks)
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

repository :: Repository
repository = pacmanRepo <> aurRepo

-- | High level 'install' command. Handles installing
-- dependencies.
install :: ( Carrier sig m
           , Member (Reader Settings) sig
           , Member (Error Failure) sig
           , Member (Lift IO) sig
           ) => NonEmptySet PkgName -> m ()
install pkgs = do
  ss <- ask
  if | not $ switch ss DeleteMakeDeps -> install' pkgs
     | otherwise -> do -- `-a` was used.
         orphansBefore <- sendM orphans
         install' pkgs
         orphansAfter <- sendM orphans
         let makeDeps = NES.fromSet (orphansAfter S.\\ orphansBefore)
         traverse_ (\mds -> sendM (notify ss . removeMakeDepsAfter_1 $ langOf ss) *> removePkgs mds) makeDeps

install' :: ( Carrier sig m
            , Member (Reader Settings) sig
            , Member (Error Failure) sig
            , Member (Lift IO) sig
            ) => NonEmptySet PkgName -> m ()
install' pkgs = do
  ss       <- ask
  unneeded <- bool
              (pure S.empty)
              (S.fromList . catMaybes <$> sendM (throttle (const isInstalled) pkgs >>= atomically . flushTQueue))
              $ shared ss NeededOnly
  let !pkgs' = NES.toSet pkgs
  if | shared ss NeededOnly && unneeded == pkgs' -> sendM . warn ss . install_2 $ langOf ss
     | otherwise -> do
         let (ignored, notIgnored) = S.partition (`S.member` ignoresOf ss) pkgs'
         installAnyway <- confirmIgnored ignored
         case NES.fromSet $ (notIgnored <> installAnyway) S.\\ unneeded of
           Nothing        -> sendM . warn ss . install_2 $ langOf ss
           Just toInstall -> do
             traverse_ (report yellow reportUnneededPackages_1) . NEL.nonEmpty $ toList unneeded
             (nons, toBuild) <- liftMaybeM (Failure connectionFailure_1) . sendM $ aurLookup (managerOf ss) toInstall
             pkgbuildDiffs toBuild
             traverse_ (report red reportNonPackages_1) . NEL.nonEmpty $ toList nons
             case NES.fromSet $ S.map (\b -> b { isExplicit = True }) toBuild of
               Nothing       -> throwError $ Failure install_2
               Just toBuild' -> do
                 sendM $ notify ss (install_5 $ langOf ss) *> hFlush stdout
                 allPkgs <- depsToInstall repository toBuild'
                 let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
                 unless (switch ss NoPkgbuildCheck) $ traverse_ (traverse_ analysePkgbuild) buildPkgs
                 reportPkgsToInstall repoPkgs buildPkgs
                 unless (switch ss DryRun) $ do
                   continue <- sendM $ optionalPrompt ss install_3
                   if | not continue -> throwError $ Failure install_4
                      | otherwise    -> do
                          traverse_ repoInstall $ NEL.nonEmpty repoPkgs
                          let !mbuildPkgs = NEL.nonEmpty buildPkgs
                          traverse_ (sendM . storePkgbuilds . fold1) mbuildPkgs
                          traverse_ buildAndInstall mbuildPkgs

-- | Determine if a package's PKGBUILD might contain malicious bash code.
analysePkgbuild :: ( Carrier sig m
                   , Member (Reader Settings) sig
                   , Member (Error Failure) sig
                   , Member (Lift IO) sig
                   ) => Buildable -> m ()
analysePkgbuild b = do
  ss <- ask
  let f = do
        yes <- sendM $ optionalPrompt ss security_6
        when yes . throwError $ Failure security_7
  case parsedPB $ b ^. field @"pkgbuild" of
    Nothing -> sendM (warn ss (security_1 (b ^. field @"name") $ langOf ss)) *> f
    Just l  -> case bannedTerms l of
      []  -> pure ()
      bts -> do
        sendM $ scold ss (security_5 (b ^. field @"name") $ langOf ss)
        sendM $ traverse_ (displayBannedTerms ss) bts
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

confirmIgnored :: ( Carrier sig m
                  , Member (Reader Settings) sig
                  , Member (Lift IO) sig
                  ) => S.Set PkgName -> m (S.Set PkgName)
confirmIgnored (toList -> ps) = do
  ss <- ask
  S.fromList <$> filterM (sendM . optionalPrompt ss . confirmIgnored_1) ps

depsToInstall :: ( Carrier sig m
                 , Member (Reader Settings) sig
                 , Member (Error Failure) sig
                 , Member (Lift IO) sig
                 ) => Repository -> NonEmptySet Buildable -> m (NonEmpty (NonEmptySet Package))
depsToInstall repo bs = do
  ss <- ask
  traverse (sendM . packageBuildable ss) (NES.toNonEmpty bs) >>= resolveDeps repo . NES.fromNonEmpty

repoInstall :: ( Carrier sig m
               , Member (Reader Settings) sig
               , Member (Error Failure) sig
               , Member (Lift IO) sig
               ) => NonEmpty Prebuilt -> m ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf)
  liftEitherM . sendM . pacman $ ["-S", "--asdeps"] <> pacOpts <> asFlag (ps ^.. each . field @"name")

buildAndInstall :: ( Carrier sig m
                   , Member (Reader Settings) sig
                   , Member (Error Failure) sig
                   , Member (Lift IO) sig
                   ) => NonEmpty (NonEmptySet Buildable) -> m ()
buildAndInstall bss = do
  pth   <- asks (either id id . cachePathOf . commonConfigOf)
  cache <- sendM $ cacheContents pth
  traverse_ (f cache) bss
  where f (Cache cache) bs = do
          ss <- ask
          let (ps, cached) = fmapEither g $ toList bs
              g b = case (b ^. super @SimplePkg) `M.lookup` cache of
                Just pp | not (switch ss ForceBuilding) -> Right pp
                _                                       -> Left b
          built <- traverse (buildPackages . NES.fromNonEmpty) $ NEL.nonEmpty ps
          traverse_ installPkgFiles $ built <> (NES.fromNonEmpty <$> NEL.nonEmpty cached)
          sendM $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies. The result of @-Ad@.
displayPkgDeps :: ( Carrier sig m
                  , Member (Reader Settings) sig
                  , Member (Error Failure) sig
                  , Member (Lift IO) sig
                  ) => NonEmptySet PkgName -> m ()
displayPkgDeps ps = do
  ss <- ask
  let f = depsToInstall repository >=> reportDeps (switch ss LowVerbosity) . partitionPkgs
  (_, goods) <- liftMaybeM (Failure connectionFailure_1) . sendM $ aurLookup (managerOf ss) ps
  traverse_ f $ NES.fromSet goods
  where reportDeps True  = sendM . uncurry reportListOfDeps
        reportDeps False = uncurry reportPkgsToInstall

reportPkgsToInstall :: ( Carrier sig m
                       , Member (Reader Settings) sig
                       , Member (Lift IO) sig
                       ) => [Prebuilt] -> [NonEmptySet Buildable] -> m ()
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

pkgbuildDiffs :: ( Carrier sig m
                 , Member (Reader Settings) sig
                 , Member (Lift IO) sig
                 ) => S.Set Buildable -> m ()
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ switch ss DiffPkgbuilds = pure ()
                   | otherwise = traverse_ displayDiff ps
          displayDiff :: ( Carrier sig m
                         , Member (Reader Settings) sig
                         , Member (Lift IO) sig
                         ) => Buildable -> m ()
          displayDiff p = do
            ss <- ask
            let pn   = p ^. field @"name"
                lang = langOf ss
            isStored <- sendM $ hasPkgbuildStored pn
            if not isStored
               then sendM . warn ss $ reportPkgbuildDiffs_1 pn lang
               else sendM $ do
                 setCurrentDirectory "/tmp"
                 let new = "/tmp/new.pb"
                 BL.writeFile new $ p ^. field @"pkgbuild" . field @"pkgbuild"
                 liftIO . warn ss $ reportPkgbuildDiffs_3 pn lang
                 diff ss (pkgbuildPath pn) $ fromAbsoluteFilePath new
