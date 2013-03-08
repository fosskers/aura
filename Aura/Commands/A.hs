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

module Aura.Commands.A
    ( installPackages
    , upgradeAURPkgs
    , aurPkgInfo
    , aurSearch
    , displayPkgDeps
    , downloadTarballs
    , displayPkgbuild ) where

import Text.Regex.PCRE ((=~))
import Control.Monad   (unless, liftM)
import Data.List       ((\\), nub, sort)

import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.Dependencies
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pkgbuilds
import Aura.Build
import Aura.Utils
import Aura.Core
import Aura.AUR

import Shell

---

installPackages :: [String] -> [String] -> Aura ()
installPackages _ []         = return ()
installPackages pacOpts pkgs = ask >>= \ss ->
  if not $ delMakeDeps ss
     then installPackages' pacOpts pkgs
     else do  -- `-a` was used with `-A`.
       orphansBefore <- getOrphans
       installPackages' pacOpts pkgs
       orphansAfter <- getOrphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ notify removeMakeDepsAfter_1
       removePkgs makeDeps pacOpts

installPackages' :: [String] -> [String] -> Aura ()
installPackages' pacOpts pkgs = ask >>= \ss -> do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
  reportIgnoredPackages ignored
  (repo,aur,nons) <- knownBadPkgCheck toInstall >>= divideByPkgType
  reportNonPackages nons
  aurPkgs <- mapM aurPkg aur >>= reportPkgbuildDiffs >>= checkHotEdit
  notify installPackages_5
  (repoDeps,aurDeps) <- catch (getDepsToInstall aurPkgs) depCheckFailure
  let repoPkgs    = nub $ repoDeps ++ repo
      pkgsAndOpts = pacOpts ++ repoPkgs
  reportPkgsToInstall repoPkgs aurDeps aurPkgs
  okay <- optionalPrompt installPackages_3
  if not okay
     then scoldAndFail installPackages_4
     else do
       unless (null repoPkgs) $ pacman (["-S","--asdeps"] ++ pkgsAndOpts)
       storePkgbuilds $ aurPkgs ++ aurDeps
       mapM_ (buildAndInstallDep pacOpts) aurDeps
       buildPackages aurPkgs >>= installPkgFiles pacOpts

knownBadPkgCheck :: [String] -> Aura [String]
knownBadPkgCheck []     = return []
knownBadPkgCheck (p:ps) = ask >>= \ss ->
  case p `lookup` wontBuildOf ss of
    Nothing -> (p :) `liftM` knownBadPkgCheck ps
    Just r  -> do
      scold $ flip knownBadPkgCheck_1 p
      putStrLnA yellow r
      okay <- optionalPrompt knownBadPkgCheck_2
      if okay then (p :) `liftM` knownBadPkgCheck ps else knownBadPkgCheck ps

depCheckFailure :: String -> Aura a
depCheckFailure m = scold installPackages_1 >> failure m

buildAndInstallDep :: [String] -> AURPkg -> Aura ()
buildAndInstallDep pacOpts pkg =
  checkHotEdit [pkg] >>= buildPackages >>=
  installPkgFiles ("--asdeps" : pacOpts)

-- | Prompts the user to edit PKGBUILDs if they ran aura with `--hotedit`.
checkHotEdit :: [AURPkg] -> Aura [AURPkg]
checkHotEdit pkgs = ask >>= check
    where check ss | mayHotEdit ss = hotEdit pkgs
                   | otherwise     = return pkgs

upgradeAURPkgs :: [String] -> [String] -> Aura ()
upgradeAURPkgs pacOpts pkgs = ask >>= \ss -> do
  let notIgnored p = splitName p `notElem` ignoredPkgsOf ss
  notify upgradeAURPkgs_1
  foreignPkgs <- filter (\(n,_) -> notIgnored n) `liftM` getForeignPackages
  pkgInfo     <- aurInfoLookup $ map fst foreignPkgs
  let aurPkgs   = filter (\(n,_) -> n `elem` map nameOf pkgInfo) foreignPkgs
      toUpgrade = filter isntMostRecent $ zip pkgInfo (map snd aurPkgs)
  devel <- develPkgCheck
  notify upgradeAURPkgs_2
  if null toUpgrade && null devel
     then warn upgradeAURPkgs_3
     else reportPkgsToUpgrade $ map prettify toUpgrade ++ devel
  installPackages pacOpts $ map (nameOf . fst) toUpgrade ++ pkgs ++ devel
    where prettify (p,v) = nameOf p ++ " : " ++ v ++ " => " ++ latestVerOf p

develPkgCheck :: Aura [String]
develPkgCheck = ask >>= \ss ->
  if rebuildDevel ss then getDevelPkgs else return []

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
  aurPkgs <- mapM (aurPkg . nameOf) info
  allDeps <- mapM determineDeps aurPkgs
  let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
  reportPkgsToInstall (n ps) (nub as) []
    where n = nub . map splitName

downloadTarballs :: [String] -> Aura ()
downloadTarballs pkgs = do
  currDir <- liftIO pwd
  filterAURPkgs pkgs >>= mapM_ (downloadTBall currDir)
    where downloadTBall path pkg = do
              notify $ flip downloadTarballs_1 pkg
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

reportPkgbuildDiffs :: [AURPkg] -> Aura [AURPkg]
reportPkgbuildDiffs [] = return []
reportPkgbuildDiffs ps = ask >>= check
    where check ss | not $ diffPkgbuilds ss = return ps
                   | otherwise = mapM_ displayDiff ps >> return ps
          displayDiff p = do
            let name = pkgNameOf p
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn $ flip reportPkgbuildDiffs_1 name
               else do
                 let new = pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds old new of
                   "" -> notify $ flip reportPkgbuildDiffs_2 name
                   d  -> do
                      warn $ flip reportPkgbuildDiffs_3 name
                      liftIO $ putStrLn $ d ++ "\n"

reportPkgsToUpgrade :: [String] -> Aura ()
reportPkgsToUpgrade pkgs = do
  lang <- langOf `liftM` ask
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
