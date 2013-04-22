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
  , depCheck
  , depsToInstall ) where

import Text.Regex.PCRE ((=~))
import Control.Monad   (filterM)
import Data.Maybe      (fromJust, isNothing)
import Data.List       ((\\), nub)

import Aura.Pacman (pacmanOutput)
import Aura.Packages.AUR
import Aura.Packages.Virtual
import Aura.Packages.Repository
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities (notNull, tripleThrd)

---

divideByPkgType ::
    PkgFilter -> PkgFilter -> [String] -> Aura ([String],[String],[String])
divideByPkgType repoPF mainPF pkgs = do
  repoPkgNames <- repoPF namesOnly
  custPkgNames <- mainPF $ namesOnly \\ repoPkgNames
  let custom   = filter (flip elem custPkgNames . splitName) pkgs
      repoPkgs = filter (flip elem repoPkgNames . splitName) pkgs
      others   = (pkgs \\ custom) \\ repoPkgs
  return (repoPkgs, custom, others)
      where namesOnly = map splitName pkgs

-- Returns the deps to be installed, or fails nicely.
depsToInstall :: Buildable a => PkgFilter -> [a] -> Aura ([String],[a])
depsToInstall _ []        = ask >>= failure . getDepsToInstall_1 . langOf
depsToInstall mainPF pkgs = ask >>= \ss -> do
  allDeps <- mapM (depCheck mainPF) pkgs
  let (ps,as,vs) = foldl groupPkgs ([],[],[]) allDeps
  necRepPkgs <- filterM mustInstall ps >>= packages
  necCusPkgs <- filterM (mustInstall . show) as
  necVirPkgs <- filterM mustInstall vs >>= packages
  let flicts = conflicts ss (necRepPkgs,necCusPkgs,necVirPkgs)
  if notNull flicts
     then failure $ unlines flicts
     else do
       let providers = map (pkgNameOf . fromJust . providerPkgOf) necVirPkgs
           repoPkgs  = map pkgNameOf necRepPkgs
       return (nub $ providers ++ repoPkgs, necCusPkgs)

-- Nick, I don't know if you intended on this or not, but using `package`
-- here instead of `buildable` forces some pretty epic polymorphism.
-- It took me quite a while to figure out how this was compiling.
-- | Returns ([RepoPackages], [CustomPackages], [VirtualPackages])
depCheck :: Buildable a => PkgFilter -> a -> Aura ([String],[a],[String])
depCheck mainPF pkg = do
  let ns   = namespaceOf pkg
      deps = concatMap (value ns) ["depends","makedepends","checkdepends"]
  (repoNames,custNames,other) <- divideByPkgType filterRepoPkgs mainPF deps
  customPkgs    <- packages custNames
  recursiveDeps <- mapM (depCheck mainPF) customPkgs
  let (rs,cs,os) = foldl groupPkgs (repoNames,customPkgs,other) recursiveDeps
  return (nub rs, nub cs, nub os)

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
conflicts :: Buildable a => Settings -> ([RepoPkg],[a],[VirtualPkg]) -> [ErrMsg]
conflicts settings (ps,as,vs) = rErr ++ aErr ++ vErr
    where rErr     = extract $ map (repoConflicts lang toIgnore) ps
          aErr     = extract $ map (customConflicts lang toIgnore) as
          vErr     = extract $ map (virtualConflicts lang toIgnore) vs
          extract  = map fromJust . filter (/= Nothing)
          lang     = langOf settings
          toIgnore = ignoredPkgsOf settings

repoConflicts :: Language -> [String] -> RepoPkg -> Maybe ErrMsg
repoConflicts = realPkgConflicts f
    where f = mostRecentVerNum . pkgInfoOf
       
-- Takes `pacman -Si` output as input.
mostRecentVerNum :: String -> String
mostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info

customConflicts :: Buildable a => Language -> [String] -> a -> Maybe ErrMsg
customConflicts = realPkgConflicts f
    where f = trueVerViaPkgbuild . namespaceOf

-- Must be called with a (f)unction that yields the version number
-- of the most up-to-date form of the package.
realPkgConflicts :: Package a => (a -> String) -> Language -> [String] ->
                    a -> Maybe ErrMsg
realPkgConflicts f lang toIgnore pkg
    | isIgnored (pkgNameOf pkg) toIgnore       = Just failMessage1
    | isVersionConflict (versionOf pkg) curVer = Just failMessage2
    | otherwise = Nothing    
    where curVer       = f pkg
          name         = pkgNameOf pkg
          reqVer       = show $ versionOf pkg
          failMessage1 = getRealPkgConflicts_2 name lang
          failMessage2 = getRealPkgConflicts_1 name curVer reqVer lang

-- This can't be generalized as easily.
virtualConflicts :: Language -> [String] -> VirtualPkg -> Maybe ErrMsg
virtualConflicts lang toIgnore pkg
    | isNothing (providerPkgOf pkg) = Just failMessage1
    | isIgnored provider toIgnore   = Just failMessage2
    | isVersionConflict (versionOf pkg) pVer = Just failMessage3
    | otherwise = Nothing
    where name         = pkgNameOf pkg
          ver          = show $ versionOf pkg
          provider     = pkgNameOf . fromJust . providerPkgOf $ pkg
          pVer         = providedVerNum pkg
          failMessage1 = getVirtualConflicts_1 name lang
          failMessage2 = getVirtualConflicts_2 name provider lang
          failMessage3 = getVirtualConflicts_3 name ver provider pVer lang

providedVerNum :: VirtualPkg -> String
providedVerNum pkg = splitVer match
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
