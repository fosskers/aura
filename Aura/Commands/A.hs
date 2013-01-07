-- Handles all `-A` operations

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

module Aura.Commands.A where

import Control.Monad (unless, liftM)
import Data.List ((\\), nub, sort)
import Text.Regex.PCRE ((=~))

import Aura.Colour.TextColouring
import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.AurConnection
import Aura.Dependencies
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pkgbuilds
import Aura.General
import Aura.Build
import Aura.Utils

import Shell

---

installPackages :: [String] -> [String] -> Aura ()
installPackages _ []         = return ()
installPackages pacOpts pkgs = ask >>= \ss -> do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
  reportIgnoredPackages ignored
  (repo,aur,nons) <- knownBadPkgCheck toInstall >>= divideByPkgType
  reportNonPackages nons
  aurPkgs <- mapM makeAURPkg aur >>= reportPkgbuildDiffs >>= checkHotEdit
  notify installPackagesMsg5
  (repoDeps,aurDeps) <- catch (getDepsToInstall aurPkgs) depCheckFailure
  let repoPkgs    = nub $ repoDeps ++ repo
      pkgsAndOpts = pacOpts ++ repoPkgs
  reportPkgsToInstall repoPkgs aurDeps aurPkgs
  okay <- optionalPrompt installPackagesMsg3
  if not okay
     then scoldAndFail installPackagesMsg4
     else do
       unless (null repoPkgs) $ pacman (["-S","--asdeps"] ++ pkgsAndOpts)
       storePkgbuilds $ aurPkgs ++ aurDeps
       mapM_ (buildAndInstallDep pacOpts) aurDeps
       pkgFiles <- buildPackages aurPkgs
       installPkgFiles pacOpts pkgFiles

knownBadPkgCheck :: [String] -> Aura [String]
knownBadPkgCheck []     = return []
knownBadPkgCheck (p:ps) = ask >>= \ss -> do
  case p `lookup` wontBuildOf ss of
    Nothing -> (p :) `liftM` knownBadPkgCheck ps
    Just r  -> do
      scold $ flip knownBadPkgCheckMsg1 p
      putStrLnA yellow r
      okay <- optionalPrompt knownBadPkgCheckMsg2
      if okay then (p :) `liftM` knownBadPkgCheck ps else knownBadPkgCheck ps

depCheckFailure :: String -> Aura a
depCheckFailure m = scold installPackagesMsg1 >> failure m

buildAndInstallDep :: [String] -> AURPkg -> Aura ()
buildAndInstallDep pacOpts pkg =
  checkHotEdit [pkg] >>= buildPackages >>=
  installPkgFiles ("--asdeps" : pacOpts)

checkHotEdit :: [AURPkg] -> Aura [AURPkg]
checkHotEdit pkgs = ask >>= check
    where check ss | mayHotEdit ss = hotEdit pkgs
                   | otherwise     = return pkgs

upgradeAURPkgs :: [String] -> [String] -> Aura ()
upgradeAURPkgs pacOpts pkgs = ask >>= \ss -> do
  let notIgnored p = splitName p `notElem` ignoredPkgsOf ss
  notify upgradeAURPkgsMsg1
  foreignPkgs <- filter (\(n,_) -> notIgnored n) `liftM` getForeignPackages
  pkgInfo <- aurInfoLookup $ map fst foreignPkgs
  let aurPkgs   = filter (\(n,_) -> n `elem` map nameOf pkgInfo) foreignPkgs
      toUpgrade = filter isntMostRecent $ zip pkgInfo (map snd aurPkgs)
  notify upgradeAURPkgsMsg2
  if null toUpgrade
     then warn upgradeAURPkgsMsg3
     else reportPkgsToUpgrade $ map prettify toUpgrade
  installPackages pacOpts $ (map (nameOf . fst) toUpgrade) ++ pkgs
    where prettify (p,v) = nameOf p ++ " : " ++ v ++ " => " ++ latestVerOf p

aurPkgInfo :: [String] -> Aura ()
aurPkgInfo pkgs = aurInfoLookup pkgs >>= mapM_ displayAurPkgInfo

displayAurPkgInfo :: PkgInfo -> Aura ()
displayAurPkgInfo info = ask >>= \ss ->
    liftIO $ putStrLn $ renderAurPkgInfo ss info ++ "\n"

renderAurPkgInfo :: Settings -> PkgInfo -> String
renderAurPkgInfo ss info = entrify ss fields entries
    where fields  = map (pcWhite ss) . infoFields . langOf $ ss
          entries = [ pcMagenta ss "aur"
                    , pcWhite ss $ nameOf info
                    , latestVerOf info
                    , outOfDateMsg (langOf ss) $ isOutOfDate info
                    , pcCyan ss $ projectURLOf info
                    , aurURLOf info
                    , licenseOf info
                    , show $ votesOf info
                    , descriptionOf info ]

aurSearch :: [String] -> Aura ()
aurSearch []    = return ()
aurSearch regex = ask >>= \ss -> do
    results <- aurSearchLookup regex
    mapM_ (liftIO . putStrLn . renderSearch ss (unwords regex)) results

renderSearch :: Settings -> String -> PkgInfo -> String
renderSearch ss r i = pcMagenta ss "aur/" ++ n ++ " " ++ v ++ "\n    " ++ d
    where c cs = case cs =~ ("(?i)" ++ r) of (b,m,a) -> b ++ cyan m ++ a
          n = c $ nameOf i
          d = c $ descriptionOf i
          v | isOutOfDate i = red $ latestVerOf i
            | otherwise     = green $ latestVerOf i

displayPkgDeps :: [String] -> Aura ()
displayPkgDeps []   = return ()
displayPkgDeps pkgs = do
  info    <- aurInfoLookup pkgs
  aurPkgs <- mapM makeAURPkg $ map nameOf info
  allDeps <- mapM determineDeps aurPkgs
  let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
  reportPkgsToInstall (n ps) (nub as) []
    where n = nub . map splitName

downloadTarballs :: [String] -> Aura ()
downloadTarballs pkgs = do
  currDir <- liftIO pwd
  filterAURPkgs pkgs >>= mapM_ (downloadTBall currDir)
    where downloadTBall path pkg = do
              notify $ flip downloadTarballsMsg1 pkg
              liftIO $ downloadSource path pkg

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild pkgs = filterAURPkgs pkgs >>= mapM_ download
      where download p = downloadPkgbuild p >>= liftIO . putStrLn

------------
-- REPORTING
------------
reportPkgsToInstall :: [String] -> [AURPkg] -> [AURPkg] -> Aura ()
reportPkgsToInstall pacPkgs aurDeps aurPkgs = do
  lang <- langOf `liftM` ask
  pl (reportPkgsToInstallMsg1 lang) (sort pacPkgs)
  pl (reportPkgsToInstallMsg2 lang) (sort $ namesOf aurDeps)
  pl (reportPkgsToInstallMsg3 lang) (sort $ namesOf aurPkgs)
      where namesOf = map pkgNameOf
            pl      = printList green cyan

reportNonPackages :: [String] -> Aura ()
reportNonPackages nons = badReport reportNonPackagesMsg1 nons 

reportIgnoredPackages :: [String] -> Aura ()
reportIgnoredPackages pkgs = do
  lang <- langOf `liftM` ask
  printList yellow cyan (reportIgnoredPackagesMsg1 lang) pkgs

reportPkgbuildDiffs :: [AURPkg] -> Aura [AURPkg]
reportPkgbuildDiffs [] = return []
reportPkgbuildDiffs ps = ask >>= check
    where check ss | not $ diffPkgbuilds ss = return ps
                   | otherwise = mapM_ displayDiff ps >> return ps
          displayDiff p = do
            let name = pkgNameOf p
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn $ flip reportPkgbuildDiffsMsg1 name
               else do
                 let new = pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds old new of
                   "" -> notify $ flip reportPkgbuildDiffsMsg2 name
                   d  -> do
                      warn $ flip reportPkgbuildDiffsMsg3 name
                      liftIO $ putStrLn $ d ++ "\n"

reportPkgsToUpgrade :: [String] -> Aura ()
reportPkgsToUpgrade pkgs = do
  lang <- langOf `liftM` ask
  printList green cyan (reportPkgsToUpgradeMsg1 lang) pkgs
