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
    ( InstallOptions(..)
    , install
    , displayPkgDeps
    ) where

import Control.Monad (unless, filterM)
import Data.Either   (partitionEithers)
import Data.List     (sort, (\\))
import Data.Maybe    (catMaybes)

import Aura.Pkgbuild.Base
import Aura.Pkgbuild.Editing
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
install _ _ [] = return ()
install opts pacOpts pkgs = ask >>= \ss ->
  if not $ delMakeDeps ss
     then install'
     else do  -- `-a` was used.
       orphansBefore <- getOrphans
       install'
       orphansAfter <- getOrphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ do
         notify removeMakeDepsAfter_1
         removePkgs makeDeps pacOpts
  where
    install' = ask >>= \ss -> do
        let toInstall = pkgs \\ ignoredPkgsOf ss
            ignored   = pkgs \\ toInstall
        reportIgnoredPackages ignored
        handler <- pbHandler
        toBuild <- badPkgCheck toInstall >>=
                   lookupPkgs (installLookup opts) >>=
                   pkgbuildDiffs >>=
                   handler
        notify install_5
        allPkgs <- catch (depsToInstall (repository opts) toBuild)
                   depCheckFailure
        let (repoPkgs,buildPkgs) = partitionPkgs allPkgs
        reportPkgsToInstall (label opts) repoPkgs buildPkgs
        continue <- optionalPrompt install_3
        if not continue
            then scoldAndFail install_4
            else do
                repoInstall pacOpts repoPkgs
                storePkgbuilds buildPkgs
                mapM_ (buildAndInstall pacOpts) buildPkgs

-- | Check a list of a package names are buildable, and mark them as explicit.
lookupPkgs :: (String -> Aura (Maybe Buildable))
           -> [String]
           -> Aura [Buildable]
lookupPkgs f pkgs = do
    (nons,okay) <- partitionEithers <$> mapM lookupBuild pkgs
    reportNonPackages nons
    return okay
  where lookupBuild pkg = maybe (Left pkg) Right <$> f pkg

-- | The user can handle PKGBUILDs in multiple ways.
-- `--hotedit` takes the highest priority.
pbHandler :: Aura ([Buildable] -> Aura [Buildable])
pbHandler = asks check
    where check ss | mayHotEdit ss      = hotEdit
                   | useCustomizepkg ss = customizepkg
                   | otherwise          = return

badPkgCheck :: [String] -> Aura [String]
badPkgCheck []     = return []
badPkgCheck (p:ps) = ask >>= \ss ->
  case p `lookup` wontBuildOf ss of
    Nothing -> (p :) <$> badPkgCheck ps
    Just r  -> do
      scold $ badPkgCheck_1 p
      putStrLnA yellow r
      okay <- optionalPrompt badPkgCheck_2
      if okay then (p :) <$> badPkgCheck ps else badPkgCheck ps

depsToInstall :: Repository -> [Buildable] -> Aura [Package]
depsToInstall repo bs = do
    allDeps <- resolveDeps repo $ map (packageBuildable . markExplicit) bs
    filterM mustInstall allDeps
  where markExplicit b = b { explicit = True }

mustInstall :: Package -> Aura Bool
mustInstall pkg | isExplicit = return True
                | otherwise  = depTest constraint
  where isExplicit = case pkgInstallType pkg of
                       Pacman _ -> False
                       Build  b -> explicit b
        constraint = pkgName pkg ++ "=" ++ pkgVersion pkg

depCheckFailure :: String -> Aura a
depCheckFailure m = scold install_1 >> failure m

repoInstall :: [String] -> [String] -> Aura ()
repoInstall _       [] = return ()
repoInstall pacOpts ps = pacman $ ["-S","--asdeps"] ++ pacOpts ++ ps

buildAndInstall :: [String] -> Buildable -> Aura ()
buildAndInstall pacOpts pkg =
  pbHandler >>= \h -> h [pkg] >>= buildPackages >>=
  installPkgFiles pacOpts'
      where pacOpts' = if explicit pkg
                       then pacOpts
                       else "--asdeps" : pacOpts

------------
-- REPORTING
------------
-- | Display dependencies.
displayPkgDeps :: InstallOptions -> [String] -> Aura ()
displayPkgDeps _ [] = return ()
displayPkgDeps opts ps = asks beQuiet >>= \quiet -> do
    bs   <- map packageBuildable . catMaybes <$> mapM (installLookup opts) ps
    pkgs <- partitionPkgs <$> resolveDeps (repository opts) bs
    reportDeps quiet pkgs
  where reportDeps True  = uncurry reportListOfDeps
        reportDeps False = uncurry (reportPkgsToInstall $ label opts)

reportPkgsToInstall :: String -> [String] -> [Buildable] -> Aura ()
reportPkgsToInstall la rps bps = asks langOf >>= \lang -> do
  pl (reportPkgsToInstall_1    lang) (sort rps)
  pl (reportPkgsToInstall_2 la lang) (sort $ map buildName bps)
      where pl = printList green cyan

reportListOfDeps :: [String] -> [Buildable] -> Aura ()
reportListOfDeps rps bps = do
  liftIO $ mapM_ putStrLn (sort rps)
  liftIO $ mapM_ putStrLn (sort $ map buildName bps)

reportNonPackages :: [String] -> Aura ()
reportNonPackages = badReport reportNonPackages_1

reportIgnoredPackages :: [String] -> Aura ()
reportIgnoredPackages pkgs = asks langOf >>= \lang ->
  printList yellow cyan (reportIgnoredPackages_1 lang) pkgs

pkgbuildDiffs :: [Buildable] -> Aura [Buildable]
pkgbuildDiffs [] = return []
pkgbuildDiffs ps = ask >>= check
    where check ss | not $ diffPkgbuilds ss = return ps
                   | otherwise = mapM_ displayDiff ps >> return ps
          displayDiff p = do
            let name = buildName p
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
