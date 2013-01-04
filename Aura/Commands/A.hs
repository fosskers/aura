-- Handles all `-A` operations

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

import Control.Monad (when, unless, liftM)
import Data.List ((\\), nub, sort)
import Text.Regex.PCRE ((=~))
import System.Exit (ExitCode)
import Data.Maybe (fromJust)

import Aura.Colour.TextColouring
import Aura.AurConnection
import Aura.Dependencies
import Aura.Languages
import Aura.Pkgbuilds
import Aura.Settings
import Aura.General
import Aura.Build
import Aura.Utils

import Utilities (fromRight)
import Shell
import Zero

---

installPackages :: Settings -> [String] -> [String] -> IO ExitCode
installPackages _ _ [] = returnFailure
installPackages ss pacOpts pkgs = do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
      lang      = langOf ss
  reportIgnoredPackages lang ignored
  (forPacman,aurPkgNames,nonPkgs) <- divideByPkgType toInstall
  reportNonPackages lang nonPkgs
  aurPackages <- mapM makeAURPkg aurPkgNames
  when (diffPkgbuilds ss) $ reportPkgbuildDiffs ss aurPackages
  aurPackages <- checkHotEdit ss aurPackages
  notify ss installPackagesMsg5
  results <- getDepsToInstall ss aurPackages
  case results of
    Left errors -> do
      printList red noColour (installPackagesMsg1 lang) errors
      returnFailure
    Right (pacmanDeps,aurDeps) -> do
      let repoPkgs    = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ repoPkgs
      reportPkgsToInstall lang repoPkgs aurDeps aurPackages 
      okay <- optionalPrompt (mustConfirm ss) (installPackagesMsg3 lang)
      if not okay
         then scoldAndFail ss installPackagesMsg4
         else do
           unless (null repoPkgs) $ do
                 pacman ss (["-S","--asdeps"] ++ pkgsAndOpts) >> return ()
           storePkgbuilds $ aurPackages ++ aurDeps
           mapM_ (buildAndInstallDep ss pacOpts) aurDeps
           pkgFiles <- buildPackages ss aurPackages
           case pkgFiles of
             Just pfs -> installPkgFiles ss pacOpts pfs
             Nothing  -> scoldAndFail ss installPackagesMsg6

buildAndInstallDep :: Settings -> [String] -> AURPkg -> IO ExitCode
buildAndInstallDep ss pacOpts pkg =
  (checkHotEdit ss [pkg] >>= buildPackages ss) ?>>=
  installPkgFiles ss ("--asdeps" : pacOpts) . fromJust

checkHotEdit :: Settings -> [AURPkg] -> IO [AURPkg]
checkHotEdit ss pkgs | mayHotEdit ss = hotEdit ss pkgs
                     | otherwise     = return pkgs

upgradeAURPkgs :: Settings -> [String] -> [String] -> IO ExitCode
upgradeAURPkgs ss pacOpts pkgs = do
  notify ss upgradeAURPkgsMsg1
  foreignPkgs <- filter (\(n,_) -> notIgnored n) `liftM` getForeignPackages
  (aurInfoLookup $ map fst foreignPkgs) ?>>= \pkgInfoEither -> do
    let pkgInfo   = fromRight pkgInfoEither
        aurPkgs   = filter (\(n,_) -> n `elem` map nameOf pkgInfo) foreignPkgs
        toUpgrade = filter isntMostRecent $ zip pkgInfo (map snd aurPkgs)
    notify ss upgradeAURPkgsMsg2
    if null toUpgrade
       then warn ss upgradeAURPkgsMsg3
       else reportPkgsToUpgrade (langOf ss) $ map prettify toUpgrade
    installPackages ss pacOpts $ (map (nameOf . fst) toUpgrade) ++ pkgs
      where notIgnored p   = splitName p `notElem` ignoredPkgsOf ss
            prettify (p,v) = nameOf p ++ " : " ++ v ++ " => " ++ latestVerOf p

aurPkgInfo :: Settings -> [String] -> IO ExitCode
aurPkgInfo ss pkgs = aurInfoLookup pkgs ?>>=
                     mapM_ (displayAurPkgInfo ss) . fromRight >>
                     returnSuccess

displayAurPkgInfo :: Settings -> PkgInfo -> IO ()
displayAurPkgInfo ss info = putStrLn $ renderAurPkgInfo ss info ++ "\n"

renderAurPkgInfo :: Settings -> PkgInfo -> String
renderAurPkgInfo ss info = entrify ss fields entries
    where fields  = infoFields $ langOf ss
          entries = [ bMagenta "aur"
                    , bForeground $ nameOf info
                    , latestVerOf info
                    , outOfDateMsg (langOf ss) $ isOutOfDate info
                    , cyan $ projectURLOf info
                    , aurURLOf info
                    , licenseOf info
                    , show $ votesOf info
                    , descriptionOf info ]

-- This is quite limited. It only accepts one word/pattern.
aurSearch :: [String] -> IO ExitCode
aurSearch []    = returnFailure
aurSearch regex = aurSearchLookup regex ?>>=
    mapM_ (putStrLn . renderSearchResult (unwords regex)) . fromRight >>
    returnSuccess

renderSearchResult :: String -> PkgInfo -> String
renderSearchResult r info = magenta "aur/" ++ n ++ " " ++ v ++ "\n    " ++ d
    where c cs = case cs =~ ("(?i)" ++ r) of (b,m,a) -> b ++ cyan m ++ a
          n = c $ nameOf info
          d = c $ descriptionOf info
          v | isOutOfDate info = red $ latestVerOf info
            | otherwise        = green $ latestVerOf info

displayPkgDeps :: Settings -> [String] -> IO ExitCode
displayPkgDeps _ []    = returnFailure
displayPkgDeps ss pkgs =
    aurInfoLookup pkgs ?>>= \infoE -> do
      aurPkgs <- mapM makeAURPkg . map nameOf . fromRight $ infoE
      allDeps <- mapM (determineDeps $ langOf ss) aurPkgs
      let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
      reportPkgsToInstall (langOf ss) (n ps) (nub as) []
      returnSuccess
          where n = nub . map splitName

downloadTarballs :: Settings -> [String] -> IO ExitCode
downloadTarballs ss pkgs = do
  currDir <- pwd
  filterAURPkgs pkgs ?>>= mapM_ (downloadTBall currDir) >> returnSuccess
    where downloadTBall path pkg = do
              notify ss $ flip downloadTarballsMsg1 pkg
              downloadSource path pkg

displayPkgbuild :: [String] -> IO ExitCode
displayPkgbuild pkgs = filterAURPkgs pkgs ?>>= mapM_ download >> returnSuccess
      where download p = downloadPkgbuild p >>= putStrLn

------------
-- REPORTING
------------
reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurDeps aurPkgs = do
  printIfThere (sort pacPkgs) $ reportPkgsToInstallMsg1 lang
  printIfThere (sort $ namesOf aurDeps) $ reportPkgsToInstallMsg2 lang
  printIfThere (sort $ namesOf aurPkgs) $ reportPkgsToInstallMsg3 lang
      where namesOf = map pkgNameOf
            printIfThere ps m = unless (null ps) $ printList green cyan m ps

reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages lang nons = badReport reportNonPackagesMsg1 lang nons 

reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages lang pkgs = printList yellow cyan msg pkgs
    where msg = reportIgnoredPackagesMsg1 lang


reportPkgbuildDiffs :: Settings -> [AURPkg] -> IO ()
reportPkgbuildDiffs ss ps | not $ diffPkgbuilds ss = return ()
                          | otherwise = mapM_ displayDiff ps
    where displayDiff p = do
            let name = pkgNameOf p
            isStored <- hasPkgbuildStored name
            if not isStored
               then warn ss $ flip reportPkgbuildDiffsMsg1 name
               else do
                 let new = pkgbuildOf p
                 old <- readPkgbuild name
                 case comparePkgbuilds old new of
                   "" -> notify ss $ flip reportPkgbuildDiffsMsg2 name
                   d  -> do
                      warn ss $ flip reportPkgbuildDiffsMsg3 name
                      putStrLn $ d ++ "\n"

reportPkgsToUpgrade :: Language -> [String] -> IO ()
reportPkgsToUpgrade lang pkgs = printList green cyan msg pkgs
    where msg = reportPkgsToUpgradeMsg1 lang
