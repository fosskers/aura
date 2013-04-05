-- Library for handling package dependencies and version conflicts.

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

module Aura.Dependencies
  ( ignoreRepos
  , divideByPkgType
  , getDepsToInstall
  , determineDeps
  ) where

import Text.Regex.PCRE ((=~))
import Control.Monad   (filterM,liftM,when)
import Data.Maybe      (fromJust, isNothing)
import Data.List        ((\\), nub, intercalate, isSuffixOf)

import Aura.Pacman (pacmanOutput)
import Aura.AUR
import Aura.Virtual
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities (notNull, tripleThrd)

---
ignoreRepos :: PkgFilter
ignoreRepos _ = return []

-- |Split a list of packages into:
--  - Repo packages.
--  - AUR packages.
--  - Other stuff.
divideByPkgType :: PkgFilter -> [String] -> Aura ([String],[String],[String])
divideByPkgType repoFilter pkgs = do
  repoPkgNames <- repoFilter namesOnly
  aurPkgNames  <- filterAURPkgs $ namesOnly \\ repoPkgNames
  let aurPkgs  = filter (flip elem aurPkgNames . splitName) pkgs
      repoPkgs = filter (flip elem repoPkgNames . splitName) pkgs
      others   = (pkgs \\ aurPkgs) \\ repoPkgs
  return (repoPkgs, aurPkgs, others)
      where namesOnly = map splitName pkgs

-- Returns the deps to be installed, or fails nicely.
getDepsToInstall :: SourcePackage a => [a] -> Aura ([String],[AURPkg])
getDepsToInstall []   = ask >>= failure . getDepsToInstall_1 . langOf
getDepsToInstall pkgs = ask >>= \ss -> do
  allDeps <- mapM determineDeps pkgs
  let (ps,as,vs) = foldl groupPkgs ([],[],[]) allDeps
  necPacPkgs <- filterM mustInstall ps >>= mapM pacmanPkg
  necAURPkgs <- filterM (mustInstall . show) as
  necVirPkgs <- filterM mustInstall vs >>= mapM virtualPkg
  let conflicts = getConflicts ss (necPacPkgs,necAURPkgs,necVirPkgs)
  if notNull conflicts
     then failure $ unlines conflicts
     else do
       let providers  = map (pkgNameOf . fromJust . providerPkgOf) necVirPkgs
           pacmanPkgs = map pkgNameOf necPacPkgs
       return (nub $ providers ++ pacmanPkgs, necAURPkgs)

-- Returns ([RepoPackages], [AURPackages], [VirtualPackages])
determineDeps :: SourcePackage a => a -> Aura ([String],[AURPkg],[String])
determineDeps pkg = do
  let ns   = namespaceOf pkg
      deps = concatMap (value ns) ["depends","makedepends","checkdepends"]
  (repoPkgNames,aurPkgNames,other) <- divideByPkgType filterRepoPkgs deps
  aurPkgs       <- mapM aurPkg aurPkgNames
  recursiveDeps <- mapM determineDeps aurPkgs
  let (rs,as,os) = foldl groupPkgs (repoPkgNames,aurPkgs,other) recursiveDeps
  return (nub rs, nub as, nub os)

-- If a package isn't installed, `pacman -T` will yield a single name.
-- Any other type of output means installation is not required. 
mustInstall :: String -> Aura Bool
mustInstall pkg = do
  necessaryDeps <- pacmanOutput ["-T",pkg]
  return $ length (words necessaryDeps) == 1

-- Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?
getConflicts :: Settings -> ([PacmanPkg],[AURPkg],[VirtualPkg]) -> [ErrMsg]
getConflicts settings (ps,as,vs) = rErr ++ aErr ++ vErr
    where rErr     = extract $ map (getPacmanConflicts lang toIgnore) ps
          aErr     = extract $ map (getAURConflicts lang toIgnore) as
          vErr     = extract $ map (getVirtualConflicts lang toIgnore) vs
          extract  = map fromJust . filter (/= Nothing)
          lang     = langOf settings
          toIgnore = ignoredPkgsOf settings

getPacmanConflicts :: Language -> [String] -> PacmanPkg -> Maybe ErrMsg
getPacmanConflicts = getRealPkgConflicts f
    where f = getMostRecentVerNum . pkgInfoOf
       
-- Takes `pacman -Si` output as input.
getMostRecentVerNum :: String -> String
getMostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info

getAURConflicts :: Language -> [String] -> AURPkg -> Maybe ErrMsg
getAURConflicts = getRealPkgConflicts f
    where f = trueVerViaPkgbuild . namespaceOf

-- Must be called with a (f)unction that yields the version number
-- of the most up-to-date form of the package.
getRealPkgConflicts :: Package a => (a -> String) -> Language -> [String] ->
                       a -> Maybe ErrMsg
getRealPkgConflicts f lang toIgnore pkg
    | isIgnored (pkgNameOf pkg) toIgnore       = Just failMessage1
    | isVersionConflict (versionOf pkg) curVer = Just failMessage2
    | otherwise = Nothing    
    where curVer       = f pkg
          name         = pkgNameOf pkg
          reqVer       = show $ versionOf pkg
          failMessage1 = getRealPkgConflicts_2 name lang
          failMessage2 = getRealPkgConflicts_1 name curVer reqVer lang

-- This can't be generalized as easily.
getVirtualConflicts :: Language -> [String] -> VirtualPkg -> Maybe ErrMsg
getVirtualConflicts lang toIgnore pkg
    | isNothing (providerPkgOf pkg) = Just failMessage1
    | isIgnored provider toIgnore   = Just failMessage2
    | isVersionConflict (versionOf pkg) pVer = Just failMessage3
    | otherwise = Nothing
    where name         = pkgNameOf pkg
          ver          = show $ versionOf pkg
          provider     = pkgNameOf . fromJust . providerPkgOf $ pkg
          pVer         = getProvidedVerNum pkg
          failMessage1 = getVirtualConflicts_1 name lang
          failMessage2 = getVirtualConflicts_2 name provider lang
          failMessage3 = getVirtualConflicts_3 name ver provider pVer lang

getProvidedVerNum :: VirtualPkg -> String
getProvidedVerNum pkg = splitVer match
    where match = info =~ ("[ ]" ++ pkgNameOf pkg ++ ">?=[0-9.]+")
          info  = pkgInfoOf . fromJust . providerPkgOf $ pkg

-- Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: VersionDemand -> String -> Bool
isVersionConflict Anything _     = False
isVersionConflict (LessThan r) c = comparableVer c >= comparableVer r
isVersionConflict (MoreThan r) c = comparableVer c <= comparableVer r
isVersionConflict (MustBe r)   c = not $ c =~ ('^' : r)
isVersionConflict (AtLeast r)  c | comparableVer c > comparableVer r = False
                                 | isVersionConflict (MustBe r) c = True
                                 | otherwise = False
