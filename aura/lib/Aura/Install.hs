{-# LANGUAGE OverloadedStrings, MultiWayIf, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TypeApplications #-}

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

-- Layer for agnostic package installation.
-- Backend for `Aura.Commands.A`.

module Aura.Install
  ( InstallOptions(..)
  , install
  , displayPkgDeps
  ) where

import           Aura.Build
import           Aura.Cache (Cache(..), cacheContents)
import           Aura.Colour
import           Aura.Core
import           Aura.Dependencies
import           Aura.Diff (diff)
import           Aura.Languages
import           Aura.Pacman
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath, diff)
import           Control.Concurrent.Async
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly (shelly, toTextIgnore, (</>), withTmpDir, cd, writefile)
import           System.IO (hFlush, stdout)

---

-- | Installation options.
data InstallOptions = InstallOptions
                      { label         :: T.Text
                      , installLookup :: Settings -> S.Set T.Text -> IO (S.Set T.Text, [Buildable])
                      , repository    :: Repository }

-- | High level 'install' command. Handles installing
-- dependencies.
install :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  InstallOptions -> S.Set T.Text -> Eff r ()
install opts pkgs = do
  ss <- ask
  if | null pkgs -> throwError $ Failure install_2
     | not $ switch ss DeleteMakeDeps -> install' opts pkgs
     | otherwise -> do -- `-a` was used.
         orphansBefore <- send orphans
         install' opts pkgs
         orphansAfter <- send orphans
         let makeDeps = orphansAfter \\ orphansBefore
         unless (null makeDeps) $ do
           send . notify ss . removeMakeDepsAfter_1 $ langOf ss
           removePkgs makeDeps

install' :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  InstallOptions -> S.Set T.Text -> Eff r ()
install' opts pkgs = do
  ss       <- ask
  unneeded <- bool (pure S.empty) (S.fromList . catMaybes <$> send (mapConcurrently isInstalled $ toList pkgs)) $ shared ss NeededOnly
  let (ignored, notIgnored) = S.partition (`elem` ignoredPkgsOf (commonConfigOf ss)) pkgs
  installAnyway <- confirmIgnored ignored
  let toInstall = (notIgnored <> installAnyway) S.\\ unneeded
  report yellow reportUnneededPackages_1 $ toList unneeded
  toBuild <- lookupPkgs (installLookup opts ss) toInstall >>= pkgbuildDiffs
  if | null toBuild && shared ss NeededOnly && unneeded == pkgs -> send . notify ss . install_2 $ langOf ss
     | null toBuild -> throwError $ Failure install_2
     | otherwise -> do
         send $ notify ss (install_5 $ langOf ss) *> hFlush stdout
         allPkgs <- depsToInstall (repository opts) toBuild
         let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
         reportPkgsToInstall (label opts) repoPkgs buildPkgs
         unless (switch ss DryRun) $ do
           continue <- send $ optionalPrompt ss install_3
           if | not continue -> throwError $ Failure install_4
              | otherwise    -> do
                  repoInstall repoPkgs
                  send . storePkgbuilds $ concat buildPkgs
                  buildAndInstall buildPkgs

-- | Give anything that was installed as a dependency the /Install Reason/ of
-- "Installed as a dependency for another package".
annotateDeps :: [Buildable] -> IO ()
annotateDeps [] = pure ()
annotateDeps bs = void . pacmanSuccess $ ["-D", "--asdeps"] <> map bldNameOf bs'
  where bs' = filter (not . isExplicit) bs

-- | Reduce a list of candidate packages to build, such that there is only one
-- instance of each "Package Base". This will ensure that split packages will
-- only be built once each. Precedence is given to packages that actually
-- match the base name (e.g. llvm50 vs llvm50-libs).
uniquePkgBase :: [[Buildable]] -> [[Buildable]]
uniquePkgBase bs = map (filter (\b -> bldNameOf b `S.member` goods)) bs
  where f a b | bldNameOf a == bldBaseNameOf a = a
              | bldNameOf b == bldBaseNameOf b = b
              | otherwise = a
        goods = S.fromList . map bldNameOf . M.elems . M.fromListWith f . map (bldBaseNameOf &&& id) $ concat bs

confirmIgnored :: (Member (Reader Settings) r, Member IO r) => S.Set T.Text -> Eff r (S.Set T.Text)
confirmIgnored (toList -> ps) = do
  ss <- ask
  S.fromList <$> filterM (send . optionalPrompt ss . confirmIgnored_1) ps

-- | Check a list of a package names are buildable, and mark them as explicit.
lookupPkgs :: (Member (Reader Settings) r, Member IO r) =>
  (S.Set T.Text -> IO (S.Set T.Text, [Buildable])) -> S.Set T.Text -> Eff r [Buildable]
lookupPkgs f pkgs = do
  (nons, okay) <- send $ f pkgs
  report red reportNonPackages_1 $ toList nons
  pure $ map (\b -> b { isExplicit = True }) okay

depsToInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> [Buildable] -> Eff r [S.Set Package]
depsToInstall repo bs = do
  ss <- ask
  traverse (send . packageBuildable ss) bs >>= resolveDeps repo

repoInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => [T.Text] -> Eff r ()
repoInstall [] = pure ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf)
  rethrow . pacman $ ["-S", "--asdeps"] <> pacOpts <> ps

buildAndInstall :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => [[Buildable]] -> Eff r ()
buildAndInstall bss = do
  pth   <- asks (either id id . cachePathOf . commonConfigOf)
  cache <- send . shelly @IO $ cacheContents pth
  traverse_ (f cache pth) bss
  where f (Cache cache) pth bs = do
          ss <- ask
          let (ps, cached) = partitionEithers $ map g bs
              g b = case bldVersionOf b >>= (\v -> SimplePkg (bldNameOf b) v `M.lookup` cache) of
                Just pp | not (switch ss ForceBuilding) -> Right $ pth </> _pkgpath pp
                _ -> Left b
          built <- buildPackages ps
          installPkgFiles $ map toTextIgnore (built <> cached)
          send $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies.
displayPkgDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  InstallOptions -> S.Set T.Text -> Eff r ()
displayPkgDeps opts ps =
  unless (null ps) $ do
    ss   <- ask
    bs   <- snd <$> send (installLookup opts ss ps)
    pkgs <- depsToInstall (repository opts) bs
    reportDeps (switch ss LowVerbosity) $ partitionPkgs pkgs
  where reportDeps True  = send . uncurry reportListOfDeps
        reportDeps False = uncurry (reportPkgsToInstall $ label opts)

reportPkgsToInstall :: (Member (Reader Settings) r, Member IO r) => T.Text -> [T.Text] -> [[Buildable]] -> Eff r ()
reportPkgsToInstall la rps bps = do
  let (explicits, deps) = partition isExplicit $ concat bps
  report green reportPkgsToInstall_1 (sort rps)
  report green reportPkgsToInstall_3 (sort $ map bldNameOf deps)
  report green (reportPkgsToInstall_2 la) (sort $ map bldNameOf explicits)

reportListOfDeps :: [T.Text] -> [[Buildable]] -> IO ()
reportListOfDeps rps bps = do
  traverse_ T.putStrLn $ sort rps
  traverse_ T.putStrLn . sort . map bldNameOf $ concat bps

pkgbuildDiffs :: (Member (Reader Settings) r, Member IO r) => [Buildable] -> Eff r [Buildable]
pkgbuildDiffs [] = pure []
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ switch ss DiffPkgbuilds = pure ps
                   | otherwise = traverse_ displayDiff ps $> ps
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
