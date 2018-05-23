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

import Aura.Build
import Aura.Colour.Text
import Aura.Core
import Aura.Dependencies
import Aura.Languages
import Aura.Monad.Aura
import Aura.Pacman
import Aura.Pkgbuild.Base
import Aura.Pkgbuild.Records
import Aura.Settings.Base
import Aura.Utils
import BasePrelude hiding (catch)

---

-- | Installation options.
data InstallOptions = InstallOptions
    { label         :: String
    , installLookup :: String -> Aura (Maybe Buildable)
    , repository    :: Repository
    }

-- | High level 'install' command. Handles installing
-- dependencies.
install :: InstallOptions  -- ^ Options.
        -> [String]        -- ^ Pacman flags.
        -> [String]        -- ^ Package names.
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

install' :: InstallOptions -> [String] -> [String] -> Aura ()
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

confirmIgnored :: [String] -> Aura [String]
confirmIgnored = filterM (optionalPrompt . confirmIgnored_1)

-- | Check a list of a package names are buildable, and mark them as explicit.
lookupPkgs :: (String -> Aura (Maybe Buildable))
           -> [String]
           -> Aura [Buildable]
lookupPkgs f pkgs = do
    (nons, okay) <- partitionEithers <$> traverse lookupBuild pkgs
    reportNonPackages nons
    pure $ markExplicit <$> okay
  where lookupBuild pkg = maybe (Left pkg) Right <$> f pkg
        markExplicit b  = b { isExplicit = True }

depsToInstall :: Repository -> [Buildable] -> Aura [Package]
depsToInstall repo = traverse packageBuildable >=> resolveDeps repo

depCheckFailure :: String -> Aura a
depCheckFailure m = scold install_1 *> failure m

repoInstall :: [String] -> [String] -> Aura ()
repoInstall _       [] = pure ()
repoInstall pacOpts ps = pacman $ ["-S", "--asdeps"] <> pacOpts <> ps

buildAndInstall :: [String] -> Buildable -> Aura ()
buildAndInstall pacOpts pkg = buildPackages [pkg] >>= installPkgFiles pacOpts'
  where pacOpts' = if isExplicit pkg then pacOpts else "--asdeps" : pacOpts

------------
-- REPORTING
------------
-- | Display dependencies.
displayPkgDeps :: InstallOptions -> [String] -> Aura ()
displayPkgDeps _ [] = pure ()
displayPkgDeps opts ps = asks beQuiet >>= \quiet -> do
    bs   <- catMaybes <$> traverse (installLookup opts) ps
    pkgs <- depsToInstall (repository opts) bs
    reportDeps quiet (partitionPkgs pkgs)
  where reportDeps True  = uncurry reportListOfDeps
        reportDeps False = uncurry (reportPkgsToInstall $ label opts)

reportPkgsToInstall :: String -> [String] -> [Buildable] -> Aura ()
reportPkgsToInstall la rps bps = asks langOf >>= \lang -> do
  pl (reportPkgsToInstall_1    lang) (sort rps)
  pl (reportPkgsToInstall_2 la lang) (sort $ baseNameOf <$> bps)
      where pl = printList green cyan

reportListOfDeps :: [String] -> [Buildable] -> Aura ()
reportListOfDeps rps bps = do
  liftIO $ traverse_ putStrLn (sort rps)
  liftIO $ traverse_ putStrLn (sort (baseNameOf <$> bps))

reportNonPackages :: [String] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

-- reportIgnoredPackages :: [String] -> Aura ()
-- reportIgnoredPackages pkgs = asks langOf >>= \lang ->
--   printList yellow cyan (reportIgnoredPackages_1 lang) pkgs

reportUnneededPackages :: [String] -> Aura ()
reportUnneededPackages pkgs = asks langOf >>= \lang ->
  printList yellow cyan (reportUnneededPackages_1 lang) pkgs

pkgbuildDiffs :: [Buildable] -> Aura [Buildable]
pkgbuildDiffs [] = pure []
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ diffPkgbuilds ss = pure ps
                   | otherwise = traverse_ displayDiff ps *> pure ps
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
                      liftIO $ putStrLn d

displayPkgbuild :: ([String] -> Aura [Maybe String]) -> [String] -> Aura ()
displayPkgbuild getPBs ps = do
  let line = yellow "\n#========== NEXT PKGBUILD ==========#\n"
  pbs <- intersperse line . catMaybes <$> getPBs ps
  traverse_ (liftIO . putStrLn) pbs
