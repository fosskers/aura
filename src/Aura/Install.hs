{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

import Control.Monad (filterM, (>=>))
import Data.Either   (partitionEithers)
import Data.List     (sort, (\\), intersperse, partition)
import Data.Foldable (traverse_)
import Data.Maybe    (catMaybes)
import Data.Monoid   ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Aura.Pkgbuild.Base
import Aura.Pkgbuild.Records
import Aura.Settings.Base
import Aura.Dependencies
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Build
import Aura.Utils
import Aura.Core
import Shelly hiding (liftIO)
import Prelude hiding (FilePath)

---

-- | Installation options.
data InstallOptions = InstallOptions
    { label         :: T.Text
    , installLookup :: T.Text -> Aura (Maybe Buildable)
    , repository    :: Repository
    }

-- | High level 'install' command. Handles installing
-- dependencies.
install :: InstallOptions  -- ^ Options.
        -> [T.Text]        -- ^ Pacman flags.
        -> [T.Text]        -- ^ Package names.
        -> Aura ()
install _ _ [] = scoldAndFail install_2
install opts pacOpts pkgs = ask >>= \ss ->
  if not $ delMakeDeps ss
     then install' opts pacOpts pkgs
     else do  -- `-a` was used.
       orphansBefore <- orphans
       install' opts pacOpts pkgs
       orphansAfter <- orphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ do
         notify removeMakeDepsAfter_1
         removePkgs makeDeps pacOpts

install' :: InstallOptions -> [T.Text] -> [T.Text] -> Aura ()
install' opts pacOpts pkgs = ask >>= \ss -> do
  unneeded <- if neededOnly ss then filterM isInstalled pkgs else pure []
  let (ignored, notIgnored) = partition (`elem` ignoredPkgsOf ss) pkgs
  installAnyway <- confirmIgnored ignored
  let toInstall  = (notIgnored <> installAnyway) \\ unneeded
  -- reportIgnoredPackages ignored  -- 2014 December  7 @ 14:52
  reportUnneededPackages unneeded
  toBuild <- lookupPkgs (installLookup opts) toInstall >>= pkgbuildDiffs
  if null toBuild
     then if neededOnly ss && unneeded == pkgs
             then notify install_2
             else scoldAndFail install_2
     else do
    notify install_5
    allPkgs <- catch (depsToInstall (repository opts) toBuild) depCheckFailure
    let (repoPkgs, buildPkgs) = partitionPkgs allPkgs
    reportPkgsToInstall (label opts) repoPkgs buildPkgs
    unless (dryRun ss) $ do
      continue <- optionalPrompt install_3
      if not continue
         then scoldAndFail install_4
         else do
        repoInstall pacOpts repoPkgs
        storePkgbuilds buildPkgs
        traverse_ (buildAndInstall pacOpts) buildPkgs

confirmIgnored :: [T.Text] -> Aura [T.Text]
confirmIgnored = filterM (optionalPrompt . confirmIgnored_1)

-- | Check a list of a package names are buildable, and mark them as explicit.
lookupPkgs :: (T.Text -> Aura (Maybe Buildable))
           -> [T.Text]
           -> Aura [Buildable]
lookupPkgs f pkgs = do
  (nons, okay) <- partitionEithers <$> traverse lookupBuild pkgs
  reportNonPackages nons
  pure $ markExplicit <$> okay
  where lookupBuild :: T.Text -> Aura (Either T.Text Buildable)
        lookupBuild pkg = maybe (Left pkg) Right <$> f pkg
        markExplicit b  = b { isExplicit = True }

depsToInstall :: Repository -> [Buildable] -> Aura [Package]
depsToInstall repo = traverse packageBuildable >=> resolveDeps repo

depCheckFailure :: T.Text -> Aura a
depCheckFailure m = scold install_1 *> failure m

repoInstall :: [T.Text] -> [T.Text] -> Aura ()
repoInstall _       [] = pure ()
repoInstall pacOpts ps = pacman $ ["-S", "--asdeps"] <> pacOpts <> ps

buildAndInstall :: [T.Text] -> Buildable -> Aura ()
buildAndInstall pacOpts pkg = buildPackages [pkg] >>= (installPkgFiles pacOpts' . map toTextIgnore)
  where pacOpts' = if isExplicit pkg then pacOpts else "--asdeps" : pacOpts

------------
-- REPORTING
------------
-- | Display dependencies.
displayPkgDeps :: InstallOptions -> [T.Text] -> Aura ()
displayPkgDeps _ [] = pure ()
displayPkgDeps opts ps = asks beQuiet >>= \quiet -> do
  bs   <- catMaybes <$> traverse (installLookup opts) ps
  pkgs <- depsToInstall (repository opts) bs
  reportDeps quiet (partitionPkgs pkgs)
  where reportDeps :: Bool -> ([T.Text], [Buildable]) -> Aura ()
        reportDeps True  = uncurry reportListOfDeps
        reportDeps False = uncurry (reportPkgsToInstall $ label opts)

reportPkgsToInstall :: T.Text -> [T.Text] -> [Buildable] -> Aura ()
reportPkgsToInstall la rps bps = asks langOf >>= \lang -> do
  pl (reportPkgsToInstall_1    lang) (sort rps)
  pl (reportPkgsToInstall_2 la lang) (sort $ baseNameOf <$> bps)
      where pl = printList green cyan

reportListOfDeps :: [T.Text] -> [Buildable] -> Aura ()
reportListOfDeps rps bps = do
  liftIO $ traverse_ IO.putStrLn (sort rps)
  liftIO $ traverse_ IO.putStrLn (sort (baseNameOf <$> bps))

reportNonPackages :: [T.Text] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

reportIgnoredPackages :: [T.Text] -> Aura ()
reportIgnoredPackages pkgs = asks langOf >>= \lang ->
  printList yellow cyan (reportIgnoredPackages_1 lang) pkgs

reportUnneededPackages :: [T.Text] -> Aura ()
reportUnneededPackages pkgs = asks langOf >>= \lang ->
  printList yellow cyan (reportUnneededPackages_1 lang) pkgs

pkgbuildDiffs :: [Buildable] -> Aura [Buildable]
pkgbuildDiffs [] = pure []
pkgbuildDiffs ps = ask >>= check
    where check :: Settings -> Aura [Buildable]
          check ss | not $ diffPkgbuilds ss = pure ps
                   | otherwise = traverse_ displayDiff ps *> pure ps
          displayDiff :: Buildable -> Aura ()
          displayDiff p = do
            let name = baseNameOf p
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn $ reportPkgbuildDiffs_1 name
               else do
                 let new = pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds name old new of
                   "" -> notify $ reportPkgbuildDiffs_2 name
                   d  -> do
                      warn $ reportPkgbuildDiffs_3 name
                      liftIO $ IO.putStrLn d

displayPkgbuild :: ([T.Text] -> Aura [Maybe Pkgbuild]) -> [T.Text] -> Aura ()
displayPkgbuild getPBs ps = do
  let line = yellow "\n#========== NEXT PKGBUILD ==========#\n"
  pbs <- intersperse line . catMaybes <$> getPBs ps
  traverse_ (liftIO . IO.putStrLn) pbs
