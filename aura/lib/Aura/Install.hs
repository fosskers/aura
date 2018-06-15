{-# LANGUAGE OverloadedStrings, Rank2Types, MultiWayIf #-}

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
-- Backend for `Aura.Commands.A` and `Aura.Commands.M`

module Aura.Install
  ( InstallOptions(..)
  , install
  , displayPkgDeps
  , displayPkgbuild
  ) where

import           Aura.Build
import           Aura.Colour.Text
import           Aura.Core
import           Aura.Dependencies
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Records
import           Aura.Settings.Base
import           Aura.Types
import           Aura.Utils
import           BasePrelude
import           Control.Concurrent.Async
import           Data.Bitraversable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly (toTextIgnore)

---

-- | Installation options.
data InstallOptions = InstallOptions
                      { label         :: T.Text
                      , installLookup :: forall m. MonadIO m => Settings -> T.Text -> m (Maybe Buildable)
                      , repository    :: Repository }

-- TODO These lists should be Sets, to guarantee that their contents are unique.
-- | High level 'install' command. Handles installing
-- dependencies.
install :: InstallOptions -> [T.Text] -> Aura (Either Failure ())
install _ []      = pure $ failure install_2
install opts pkgs = ask >>= f
  where
    f ss | not $ switch ss DeleteMakeDeps = install' opts pkgs
         | otherwise = do -- `-a` was used.
             orphansBefore <- orphans
             install' opts pkgs
             orphansAfter <- orphans
             let makeDeps = orphansAfter \\ orphansBefore
             if | null makeDeps -> pure $ Right ()
                | otherwise -> do
                    notify . removeMakeDepsAfter_1 $ langOf ss
                    removePkgs makeDeps

install' :: InstallOptions -> [T.Text] -> Aura (Either Failure ())
install' opts pkgs = do
  ss       <- ask
  unneeded <- bool (pure []) (catMaybes <$> liftIO (mapConcurrently isInstalled pkgs)) $ shared ss NeededOnly
  let (ignored, notIgnored) = partition (`elem` ignoredPkgsOf (commonConfigOf ss)) pkgs
  installAnyway <- confirmIgnored ignored
  let toInstall = (notIgnored <> installAnyway) \\ unneeded
  -- reportIgnoredPackages ignored  -- 2014 December  7 @ 14:52
  reportUnneededPackages unneeded
  toBuild <- lookupPkgs (installLookup opts ss) toInstall >>= pkgbuildDiffs
  if | null toBuild && shared ss NeededOnly && unneeded == pkgs -> fmap Right . notify . install_2 $ langOf ss
     | null toBuild -> pure $ failure install_2
     | otherwise -> do
         notify . install_5 $ langOf ss
         depsToInstall (repository opts) toBuild >>= fmap join . bitraverse pure (f ss)
  where f ss allPkgs = do
          let (repoPkgs, buildPkgs) = partitionPkgs allPkgs
          reportPkgsToInstall (label opts) repoPkgs buildPkgs
          if | switch ss DryRun -> pure $ Right ()
             | otherwise -> do
                 continue <- optionalPrompt ss install_3
                 if | not continue -> pure $ failure install_4
                    | otherwise    -> do
                        repoInstall repoPkgs
                        storePkgbuilds buildPkgs
                        buildAndInstall buildPkgs

confirmIgnored :: [T.Text] -> Aura [T.Text]
confirmIgnored ps = do
  ss <- ask
  filterM (optionalPrompt ss . confirmIgnored_1) ps

-- | Check a list of a package names are buildable, and mark them as explicit.
lookupPkgs :: (T.Text -> IO (Maybe Buildable)) -> [T.Text] -> Aura [Buildable]
lookupPkgs f pkgs = do
  (nons, okay) <- partitionEithers <$> liftIO (mapConcurrently lookupBuild pkgs)
  reportNonPackages nons
  pure $ map markExplicit okay
  where lookupBuild pkg = maybe (Left pkg) Right <$> f pkg
        markExplicit b  = b { isExplicit = True }

depsToInstall :: Repository -> [Buildable] -> Aura (Either Failure [Package])
depsToInstall repo bs = do
  ss <- ask
  traverse (packageBuildable ss) bs >>= resolveDeps repo

repoInstall :: [T.Text] -> Aura (Either Failure ())
repoInstall [] = pure $ Right ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf)
  pacman $ ["-S", "--asdeps"] <> pacOpts <> ps

buildAndInstall :: [Buildable] -> Aura (Either Failure ())
buildAndInstall []     = pure $ Right ()
buildAndInstall (b:bs) = do
  eps <- buildPackages [b]
  fmap join . for eps $ \ps -> do
    installPkgFiles asDeps $ map toTextIgnore ps
    buildAndInstall bs
  where asDeps = if isExplicit b then Nothing else Just "--asdeps"

------------
-- REPORTING
------------
-- | Display dependencies.
displayPkgDeps :: InstallOptions -> [T.Text] -> Aura (Either Failure ())
displayPkgDeps _ [] = pure $ Right ()
displayPkgDeps opts ps = do
  ss   <- ask
  bs   <- catMaybes <$> liftIO (mapConcurrently (installLookup opts ss) ps)
  pkgs <- depsToInstall (repository opts) bs
  bitraverse pure (reportDeps (switch ss LowVerbosity) . partitionPkgs) pkgs
  where reportDeps True  = uncurry reportListOfDeps
        reportDeps False = uncurry (reportPkgsToInstall $ label opts)

reportPkgsToInstall :: T.Text -> [T.Text] -> [Buildable] -> Aura ()
reportPkgsToInstall la rps bps = asks langOf >>= \lang -> do
  pl (reportPkgsToInstall_1    lang) (sort rps)
  pl (reportPkgsToInstall_2 la lang) (sort $ map baseNameOf bps)
      where pl = printList green cyan

reportListOfDeps :: MonadIO m => [T.Text] -> [Buildable] -> m ()
reportListOfDeps rps bps = liftIO $ do
  traverse_ T.putStrLn $ sort rps
  traverse_ T.putStrLn . sort $ map baseNameOf bps

reportNonPackages :: [T.Text] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

reportUnneededPackages :: [T.Text] -> Aura ()
reportUnneededPackages pkgs = asks langOf >>= \lang ->
  printList yellow cyan (reportUnneededPackages_1 lang) pkgs

pkgbuildDiffs :: [Buildable] -> Aura [Buildable]
pkgbuildDiffs [] = pure []
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ switch ss DiffPkgbuilds = pure ps
                   | otherwise = traverse_ displayDiff ps $> ps
          displayDiff :: Buildable -> Aura ()
          displayDiff p = do
            let name = baseNameOf p
            lang     <- asks langOf
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn $ reportPkgbuildDiffs_1 name lang
               else do
                 let new = _pkgbuild $ pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds name old new of
                   "" -> notify $ reportPkgbuildDiffs_2 name lang
                   d  -> do
                      warn $ reportPkgbuildDiffs_3 name lang
                      liftIO $ T.putStrLn d

displayPkgbuild :: ([T.Text] -> Aura [Maybe T.Text]) -> [T.Text] -> Aura ()
displayPkgbuild getPBs ps = do
  let line = yellow "\n#========== NEXT PKGBUILD ==========#\n"
  pbs <- intersperse line . catMaybes <$> getPBs ps
  traverse_ (liftIO . T.putStrLn) pbs
