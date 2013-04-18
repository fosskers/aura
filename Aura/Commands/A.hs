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
    ( install
    , upgradeAURPkgs
    , aurPkgInfo
    , aurSearch
    , displayPkgDeps
    , downloadTarballs
    , displayPkgbuild ) where

import Text.Regex.PCRE ((=~))
import Data.List       (nub, nubBy)

import qualified Aura.Install as I

import Aura.Settings.Base
import Aura.Dependencies
import Aura.Packages.AUR
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Core

import Shell

---

install :: [String] -> [String] -> Aura ()
install pacOpts pkgs = I.install b filterAURPkgs pacOpts pkgs
    where b = package :: String -> Aura AURPkg  -- Force the type.

upgradeAURPkgs :: [String] -> [String] -> Aura ()
upgradeAURPkgs pacOpts pkgs = ask >>= \ss -> do
  let notIgnored p = splitName p `notElem` ignoredPkgsOf ss
  notify upgradeAURPkgs_1
  foreignPkgs <- filter (\(n,_) -> notIgnored n) `fmap` getForeignPackages
  pkgInfo     <- aurInfoLookup $ map fst foreignPkgs
  let aurPkgs   = filter (\(n,_) -> n `elem` map nameOf pkgInfo) foreignPkgs
      toUpgrade = filter isntMostRecent $ zip pkgInfo (map snd aurPkgs)
  devel <- develPkgCheck
  notify upgradeAURPkgs_2
  if null toUpgrade && null devel
     then warn upgradeAURPkgs_3
     else reportPkgsToUpgrade $ map prettify toUpgrade ++ devel
  install pacOpts $ map (nameOf . fst) toUpgrade ++ pkgs ++ devel
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
    where fields  = map white . infoFields . langOf $ ss
          entries = [ magenta "aur"
                    , white $ nameOf info
                    , latestVerOf info
                    , outOfDateMsg (isOutOfDate info) $ langOf ss
                    , cyan $ projectURLOf info
                    , aurURLOf info
                    , licenseOf info
                    , yellow . show . votesOf $ info
                    , descriptionOf info ]

aurSearch :: [String] -> Aura ()
aurSearch []    = return ()
aurSearch regex = do
    results <- aurSearchLookup regex
    mapM_ (liftIO . putStrLn . renderSearch (unwords regex)) results

renderSearch :: String -> PkgInfo -> String
renderSearch r i = repo ++ n ++ " " ++ v ++ " (" ++ l ++ ")\n    " ++ d
    where c cl cs = case cs =~ ("(?i)" ++ r) of
                      (b,m,a) -> cl b ++ bCyan m ++ cl a
          repo = magenta "aur/"
          n = c bForeground $ nameOf i
          d = c noColour $ descriptionOf i
          l = yellow . show . votesOf $ i  -- `l` for likes?
          v | isOutOfDate i = red $ latestVerOf i
            | otherwise     = green $ latestVerOf i

displayPkgDeps :: [String] -> Aura ()
displayPkgDeps []   = return ()
displayPkgDeps pkgs = do
  info    <- aurInfoLookup pkgs
  aurPkgs <- (mapM (package . nameOf) info) :: Aura [AURPkg]
  allDeps <- mapM (depCheck filterAURPkgs) aurPkgs
  let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
  I.reportPkgsToInstall (n ps) (nubBy sameName as) []
    where n = nub . map splitName
          sameName a b = pkgNameOf a == pkgNameOf b

downloadTarballs :: [String] -> Aura ()
downloadTarballs pkgs = do
  currDir <- liftIO pwd
  filterAURPkgs pkgs >>= mapM_ (downloadTBall currDir)
    where downloadTBall path pkg = do
              notify $ downloadTarballs_1 pkg
              liftIO $ sourceTarball path pkg

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild pkgs = filterAURPkgs pkgs >>= mapM_ dnload
      where dnload p = downloadPkgbuild p >>= liftIO . putStrLn

isntMostRecent :: (PkgInfo,String) -> Bool
isntMostRecent (info,v) = trueVer > currVer
  where trueVer = comparableVer $ latestVerOf info
        currVer = comparableVer v

------------
-- REPORTING
------------
reportPkgsToUpgrade :: [String] -> Aura ()
reportPkgsToUpgrade pkgs = do
  lang <- langOf `fmap` ask
  printList green cyan (reportPkgsToUpgrade_1 lang) pkgs
