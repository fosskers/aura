{-# LANGUAGE OverloadedStrings, MultiWayIf, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TypeApplications #-}

-- |
-- Module    : Aura.Install
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Layer for AUR package installation.
-- Backend for `Aura.Commands.A`.

module Aura.Install
  ( install
  , displayPkgDeps
  ) where

import           Aura.Build
import           Aura.Cache (Cache(..), cacheContents)
import           Aura.Colour
import           Aura.Core
import           Aura.Dependencies
import           Aura.Diff (diff)
import           Aura.Languages
import           Aura.Packages.AUR (aurLookup, aurRepo)
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Pkgbuild.Security
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath, diff)
import           Control.Compactable (fmapEither)
import           Control.Concurrent.Async
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import           Data.Semigroup.Foldable (fold1)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.Bash.Pretty (prettyText)
import           Language.Bash.Syntax (Statement)
import           Shelly (shelly, (</>), withTmpDir, cd, writefile, whenM)
import           System.IO (hFlush, stdout)

---

repository :: Repository
repository = pacmanRepo <> aurRepo

-- | High level 'install' command. Handles installing
-- dependencies.
install :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
install pkgs = do
  ss <- ask
  if | not $ switch ss DeleteMakeDeps -> install' pkgs
     | otherwise -> do -- `-a` was used.
         orphansBefore <- send orphans
         install' pkgs
         orphansAfter <- send orphans
         let makeDeps = NES.fromSet (orphansAfter S.\\ orphansBefore)
         traverse_ (\mds -> send (notify ss . removeMakeDepsAfter_1 $ langOf ss) *> removePkgs mds) makeDeps

install' :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
install' pkgs = do
  ss       <- ask
  unneeded <- bool (pure S.empty) (S.fromList . catMaybes <$> send (mapConcurrently isInstalled $ toList pkgs)) $ shared ss NeededOnly
  let !pkgs' = NES.toSet pkgs
  if | shared ss NeededOnly && unneeded == pkgs' -> send . warn ss . install_2 $ langOf ss
     | otherwise -> do
         let (ignored, notIgnored) = S.partition (`elem` ignoredPkgsOf (commonConfigOf ss)) pkgs'
         installAnyway <- confirmIgnored ignored
         case NES.fromSet $ (notIgnored <> installAnyway) S.\\ unneeded of
           Nothing        -> send . warn ss . install_2 $ langOf ss
           Just toInstall -> do
             traverse_ (report yellow reportUnneededPackages_1) . NEL.nonEmpty $ toList unneeded
             (nons, toBuild) <- send $ aurLookup ss toInstall
             pkgbuildDiffs toBuild
             traverse_ (report red reportNonPackages_1) . NEL.nonEmpty $ toList nons
             case NES.fromSet $ S.map (\b -> b { isExplicit = True }) toBuild of
               Nothing       -> throwError $ Failure install_2
               Just toBuild' -> do
                 send $ notify ss (install_5 $ langOf ss) *> hFlush stdout
                 allPkgs <- depsToInstall repository toBuild'
                 let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
                 traverse_ (traverse_ analysePkgbuild) buildPkgs
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
analysePkgbuild :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Buildable -> Eff r ()
analysePkgbuild b = do
  ss <- ask
  send $ case parsedPB $ pkgbuildOf b of
    Nothing -> warn ss (security_1 (bldNameOf b) $ langOf ss)
    Just l  -> case bannedTerms l of
      []  -> pure ()
      bts -> do
        scold ss (security_5 (bldNameOf b) $ langOf ss)
        traverse_ (displayBannedTerms ss) bts
  whenM (send $ optionalPrompt ss security_6) . throwError $ Failure security_7

displayBannedTerms :: Settings -> (Statement, NonEmptySet BannedTerm) -> IO ()
displayBannedTerms ss (stmt, bts) = do
  putStrLn $ prettyText stmt
  traverse_ (\b -> warn ss $ reportExploit b lang) bts
  where lang = langOf ss

-- | Give anything that was installed as a dependency the /Install Reason/ of
-- "Installed as a dependency for another package".
annotateDeps :: NonEmptySet Buildable -> IO ()
annotateDeps bs = void . pacmanSuccess $ ["-D", "--asdeps"] <> asFlag (map bldNameOf bs')
  where bs' = filter (not . isExplicit) $ toList bs

-- | Reduce a list of candidate packages to build, such that there is only one
-- instance of each "Package Base". This will ensure that split packages will
-- only be built once each. Precedence is given to packages that actually
-- match the base name (e.g. llvm50 vs llvm50-libs).
uniquePkgBase :: [NonEmptySet Buildable] -> [NonEmptySet Buildable]
uniquePkgBase bs = mapMaybe (NES.fromSet . S.filter (\b -> bldNameOf b `S.member` goods) . NES.toSet) bs
  where f a b | bldNameOf a == bldBaseNameOf a = a
              | bldNameOf b == bldBaseNameOf b = b
              | otherwise = a
        goods = S.fromList . map bldNameOf . M.elems . M.fromListWith f $ map (bldBaseNameOf &&& id) bs'
        bs'   = foldMap toList bs

confirmIgnored :: (Member (Reader Settings) r, Member IO r) => S.Set PkgName -> Eff r (S.Set PkgName)
confirmIgnored (toList -> ps) = do
  ss <- ask
  S.fromList <$> filterM (send . optionalPrompt ss . confirmIgnored_1) ps

depsToInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Buildable -> Eff r (NonEmpty (NonEmptySet Package))
depsToInstall repo bs = do
  ss <- ask
  traverse (send . packageBuildable ss) (NES.toNonEmpty bs) >>= resolveDeps repo . NES.fromNonEmpty

repoInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => NonEmpty PkgName -> Eff r ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf)
  rethrow . pacman $ ["-S", "--asdeps"] <> pacOpts <> asFlag ps

buildAndInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmpty (NonEmptySet Buildable) -> Eff r ()
buildAndInstall bss = do
  pth   <- asks (either id id . cachePathOf . commonConfigOf)
  cache <- send . shelly @IO $ cacheContents pth
  traverse_ (f cache) bss
  where f (Cache cache) bs = do
          ss <- ask
          let (ps, cached) = fmapEither g $ toList bs
              g b = case bldVersionOf b >>= (\v -> SimplePkg (bldNameOf b) v `M.lookup` cache) of
                Just pp | not (switch ss ForceBuilding) -> Right pp
                _ -> Left b
          built <- traverse (buildPackages . NES.fromNonEmpty) $ NEL.nonEmpty ps
          traverse_ installPkgFiles $ built <> (NES.fromNonEmpty <$> NEL.nonEmpty cached)
          send $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies. The result of @-Ad@.
displayPkgDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
displayPkgDeps ps = do
  ss <- ask
  let f = depsToInstall repository >=> reportDeps (switch ss LowVerbosity) . partitionPkgs
  send (aurLookup ss ps) >>= traverse_ f . NES.fromSet . snd
  where reportDeps True  = send . uncurry reportListOfDeps
        reportDeps False = uncurry reportPkgsToInstall

reportPkgsToInstall :: (Member (Reader Settings) r, Member IO r) =>
   [PkgName] -> [NonEmptySet Buildable] -> Eff r ()
reportPkgsToInstall rps bps = do
  let (explicits, deps) = partition isExplicit $ foldMap toList bps
  traverse_ (report green reportPkgsToInstall_1) . NEL.nonEmpty $ sort rps
  traverse_ (report green reportPkgsToInstall_3) . NEL.nonEmpty . sort $ map bldNameOf deps
  traverse_ (report green reportPkgsToInstall_2) . NEL.nonEmpty . sort $ map bldNameOf explicits

reportListOfDeps :: [PkgName] -> [NonEmptySet Buildable] -> IO ()
reportListOfDeps rps bps = do
  traverse_ (T.putStrLn . _pkgname) $ sort rps
  traverse_ (T.putStrLn . _pkgname) . sort . map bldNameOf $ foldMap toList bps

pkgbuildDiffs :: (Member (Reader Settings) r, Member IO r) => S.Set Buildable -> Eff r ()
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ switch ss DiffPkgbuilds = pure ()
                   | otherwise = traverse_ displayDiff ps
          displayDiff :: (Member (Reader Settings) r, Member IO r) => Buildable -> Eff r ()
          displayDiff p = do
            ss <- ask
            let name = bldNameOf p
                lang = langOf ss
            isStored <- send $ hasPkgbuildStored name
            if not isStored
               then send . warn ss $ reportPkgbuildDiffs_1 name lang
               else send . shelly @IO . withTmpDir $ \dir -> do
                 cd dir
                 let new = dir </> ("new.pb" :: T.Text)
                 writefile new . _pkgbuild $ pkgbuildOf p
                 liftIO . warn ss $ reportPkgbuildDiffs_3 name lang
                 diff ss (pkgbuildPath name) new
