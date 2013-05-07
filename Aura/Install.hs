{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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
    ( install
    , reportPkgsToInstall ) where

import Control.Monad (unless)
import Data.List     (sort, (\\))

import Aura.Pkgbuild.Records
import Aura.Pkgbuild.Editing
import Aura.Settings.Base
import Aura.Dependencies
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Build
import Aura.Utils
import Aura.Core

---

-- | High level 'install' command. Handles installing
-- dependencies.
install, install' :: (Package p, Buildable b) =>
                     (String -> Aura b)  -- ^ Constructor for Buildable data
                  -> (Settings -> p -> Maybe ErrMsg)
                  -> BuildHandle  -- ^ For filtering packages and building deps.
                  -> [String]     -- ^ Pacman flags.
                  -> [String]     -- ^ Package names.
                  -> Aura ()
install _ _ _ _ [] = return ()
install custom subConflict bh pacOpts pkgs = ask >>= \ss ->
  if not $ delMakeDeps ss
     then install' custom subConflict bh pacOpts pkgs
     else do  -- `-a` was used.
       orphansBefore <- getOrphans
       install' custom subConflict bh pacOpts pkgs
       orphansAfter <- getOrphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ notify removeMakeDepsAfter_1
       removePkgs makeDeps pacOpts

install' custom subConflict bh pacOpts pkgs = ask >>= \ss -> do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
      mainPkgs  = initialPF bh
  reportIgnoredPackages ignored
  (_,okay,nons) <- badPkgCheck toInstall >>= divideByPkgType ignoreRepos mainPkgs
  reportNonPackages nons
  handler <- pbHandler
  toBuild <- mapM custom okay >>= pkgbuildDiffs >>= handler
  notify install_5
  (subDeps,mainDeps) <- catch (depsToInstall subConflict bh toBuild) depCheckFailure
  reportPkgsToInstall bh subDeps mainDeps toBuild
  continue <- optionalPrompt install_3
  if not continue
     then scoldAndFail install_4
     else do
       unless (null subDeps) $ subBuild bh subDeps
       storePkgbuilds $ toBuild ++ mainDeps
       mapM_ (buildAndInstallDep pacOpts) mainDeps
       buildPackages toBuild >>= installPkgFiles pacOpts

-- | The user can handle PKGBUILDs in multiple ways.
-- `--hotedit` takes the highest priority.
pbHandler :: Buildable b => Aura ([b] -> Aura [b])
pbHandler = ask >>= check
    where check ss | mayHotEdit ss      = return hotEdit
                   | useCustomizepkg ss = return customizepkg
                   | otherwise          = return return

badPkgCheck :: [String] -> Aura [String]
badPkgCheck []     = return []
badPkgCheck (p:ps) = ask >>= \ss ->
  case p `lookup` wontBuildOf ss of
    Nothing -> (p :) `fmap` badPkgCheck ps
    Just r  -> do
      scold $ badPkgCheck_1 p
      putStrLnA yellow r
      okay <- optionalPrompt badPkgCheck_2
      if okay then (p :) `fmap` badPkgCheck ps else badPkgCheck ps

depCheckFailure :: String -> Aura a
depCheckFailure m = scold install_1 >> failure m

buildAndInstallDep :: Buildable b => [String] -> b -> Aura ()
buildAndInstallDep pacOpts pkg =
  pbHandler >>= \h -> h [pkg] >>= buildPackages >>=
  installPkgFiles ("--asdeps" : pacOpts)

------------
-- REPORTING
------------
reportPkgsToInstall :: (Package p1, Package p2, Package p3) =>
                       BuildHandle -> [p1] -> [p2] -> [p3] -> Aura ()
reportPkgsToInstall bh sd md mp = langOf `fmap` ask >>= \lang -> do
  pl (reportPkgsToInstall_1    lang) (sort $ map pkgNameOf sd)
  pl (reportPkgsToInstall_2 la lang) (sort $ map pkgNameOf md)
  pl (reportPkgsToInstall_3 la lang) (sort $ map pkgNameOf mp)
      where pl = printList green cyan
            la = pkgLabel bh

reportNonPackages :: [String] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

reportIgnoredPackages :: [String] -> Aura ()
reportIgnoredPackages pkgs = langOf `fmap` ask >>= \lang ->
  printList yellow cyan (reportIgnoredPackages_1 lang) pkgs

pkgbuildDiffs :: Buildable b => [b] -> Aura [b]
pkgbuildDiffs [] = return []
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ diffPkgbuilds ss = return ps
                   | otherwise = mapM_ displayDiff ps >> return ps
          displayDiff p = do
            let name = pkgNameOf p
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn $ reportPkgbuildDiffs_1 name
               else do
                 let new = pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds old new of
                   "" -> notify $ reportPkgbuildDiffs_2 name
                   d  -> do
                      warn $ reportPkgbuildDiffs_3 name
                      liftIO $ putStrLn $ d ++ "\n"
