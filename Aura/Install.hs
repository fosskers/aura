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

import Data.List     (sort,nub,(\\))
import Control.Monad (unless,liftM)

import Aura.Pacman (pacman)
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

-- | The user can handle PKGBUILDs in multiple ways.
-- `--hotedit` takes the highest priority.
pbHandler :: Buildable a => Aura ([a] -> Aura [a])
pbHandler = ask >>= check
    where check ss | mayHotEdit ss      = return hotEdit
                   | useCustomizepkg ss = return customizepkg
                   | otherwise          = return return

-- | High level 'install' command. Handles installing
-- dependencies.
install :: [String] -- ^ Package options
        -> [String] -- ^ Packages to install
        -> Aura ()
install _ []         = return ()
install pacOpts pkgs = ask >>= \ss ->
  if not $ delMakeDeps ss
     then install' pacOpts pkgs
     else do  -- `-a` was used with `-A`.
       orphansBefore <- getOrphans
       install' pacOpts pkgs
       orphansAfter <- getOrphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ notify removeMakeDepsAfter_1
       removePkgs makeDeps pacOpts

install' :: [String] -> [String] -> Aura ()
install' pacOpts pkgs = ask >>= \ss -> do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
  reportIgnoredPackages ignored
  (_,aur,nons) <- knownBadPkgCheck toInstall >>= divideByPkgType ignoreRepos
  reportNonPackages nons
  handler <- pbHandler
  aurPkgs <- mapM package aur >>= reportPkgbuildDiffs >>= handler
  notify install_5
  (repoDeps,aurDeps) <- catch (getDepsToInstall aurPkgs) depCheckFailure
  let repoPkgs    = nub repoDeps
      pkgsAndOpts = pacOpts ++ repoPkgs
  reportPkgsToInstall repoPkgs aurDeps aurPkgs
  okay <- optionalPrompt install_3
  if not okay
     then scoldAndFail install_4
     else do
       unless (null repoPkgs) $ pacman (["-S","--asdeps"] ++ pkgsAndOpts)
       storePkgbuilds $ aurPkgs ++ aurDeps
       mapM_ (buildAndInstallDep handler pacOpts) aurDeps
       buildPackages aurPkgs >>= installPkgFiles pacOpts

knownBadPkgCheck :: [String] -> Aura [String]
knownBadPkgCheck []     = return []
knownBadPkgCheck (p:ps) = ask >>= \ss ->
  case p `lookup` wontBuildOf ss of
    Nothing -> (p :) `liftM` knownBadPkgCheck ps
    Just r  -> do
      scold $ knownBadPkgCheck_1 p
      putStrLnA yellow r
      okay <- optionalPrompt knownBadPkgCheck_2
      if okay then (p :) `liftM` knownBadPkgCheck ps else knownBadPkgCheck ps

depCheckFailure :: String -> Aura a
depCheckFailure m = scold install_1 >> failure m

buildAndInstallDep :: (Buildable a, Show a) =>
                      ([a] -> Aura [a]) -> [String] -> a -> Aura ()
buildAndInstallDep handler pacOpts pkg =
  handler [pkg] >>= buildPackages >>=
  installPkgFiles ("--asdeps" : pacOpts)

------------
-- REPORTING
------------
reportPkgsToInstall :: Buildable a => [String] -> [a] -> [a] -> Aura ()
reportPkgsToInstall pacPkgs aurDeps aurPkgs = do
  lang <- langOf `liftM` ask
  pl (reportPkgsToInstall_1 lang) (sort pacPkgs)
  pl (reportPkgsToInstall_2 lang) (sort $ namesOf aurDeps)
  pl (reportPkgsToInstall_3 lang) (sort $ namesOf aurPkgs)
      where namesOf = map pkgNameOf
            pl      = printList green cyan

reportNonPackages :: [String] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

reportIgnoredPackages :: [String] -> Aura ()
reportIgnoredPackages pkgs = do
  lang <- langOf `liftM` ask
  printList yellow cyan (reportIgnoredPackages_1 lang) pkgs

reportPkgbuildDiffs :: Buildable a => [a] -> Aura [a]
reportPkgbuildDiffs [] = return []
reportPkgbuildDiffs ps = ask >>= check
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
