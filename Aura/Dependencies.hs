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

import qualified Data.Map.Lazy as M

import Control.Monad   (filterM)
import Data.Maybe      (fromJust, mapMaybe)
import Data.List       ((\\))

import Aura.Pacman        (pacmanOutput)
import Aura.Packages.Repository
import Aura.Packages.Virtual
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities (notNull)

---

{- NOTE

`Main Packages` are either AUR or ABS packages.
`Sub Packages`  are either ABS or Repository packages.

These can (or should) freely overlap.

-}

type DepMap a = M.Map String a

-- `PkgFilter` is in the Aura Monad, so this function must be too.
divideByPkgType :: PkgFilter -> PkgFilter -> [String]
                -> Aura ([String],[String],[String])
divideByPkgType subPF' mainPF' pkgs = do
  subPkgNames  <- subPF' namesOnly
  mainPkgNames <- mainPF' $ namesOnly \\ subPkgNames
  let mains  = filter (flip elem mainPkgNames . splitName) pkgs
      subs   = filter (flip elem subPkgNames  . splitName) pkgs
      others = (pkgs \\ mains) \\ subs
  return (subs, mains, others)
      where namesOnly = map splitName pkgs

-- | Returns all dependencies to be installed, or fails nicely.
depsToInstall :: (Package p, Buildable b)
              => (Settings -> p -> Maybe ErrMsg) -> BuildHandle -> [b]
              -> Aura ([p],[b])
depsToInstall _ _ [] = ask >>= failure . getDepsToInstall_1 . langOf
depsToInstall subConflict bh pkgs = ask >>= \ss -> do
  (subs,mains,virts) <- depCheck bh pkgs
  necSubPkgs  <- filterM (mustInstall . show) subs
  necMainPkgs <- filterM (mustInstall . show) mains
  necVirPkgs  <- filterM mustInstall virts >>= packages
  let flicts = conflicts subConflict ss (necSubPkgs,necMainPkgs,necVirPkgs)
  if notNull flicts
     then failure $ unlines flicts
     else do
       let providers = map (pkgNameOf . fromJust . providerPkgOf) necVirPkgs
       providers' <- packages $ (providers \\ map pkgNameOf necSubPkgs)
       return (providers' ++ necSubPkgs, necMainPkgs)

depCheck :: (Package p, Buildable b)
         => BuildHandle -> [b] -> Aura ([p],[b],[String])
depCheck bh ps = depCheck' bh ps (M.empty, M.empty, M.empty)

-- Recall that using `show` on a Package gives a String in the form:
-- `name>=version`  where `>=` could really be any of the comparison operators.
-- Virtual package names taken from PKGBUILDs should already be in this form,
-- so they need not be `show`n.
depCheck' :: (Package p, Buildable b)
          => BuildHandle -> [b] -> (DepMap p, DepMap b, DepMap String)
          -> Aura ([p],[b],[String])
depCheck' _ [] (s,m,v)      = return (M.elems s, M.elems m, M.elems v)
depCheck' bh (p:ps) (s,m,v) = do
  let ns    = namespaceOf p
      df dm = filter (\d -> M.notMember d dm)
      deps  = concatMap (value ns) ["depends","makedepends","checkdepends"]
      deps' = df v . df s . df m $ deps
  (subNames,mainNames,other) <- divideByPkgType (subPF bh) (mainPF bh) deps'
  mainPkgs <- packages mainNames
  subPkgs  <- packages subNames
  depCheck' bh (ps ++ mainPkgs) (i show s subPkgs, i show m mainPkgs, i id v other)
      where i f = foldl (\acc x -> M.insert (f x) x acc)

-- Moving to a libalpm backend will make this less hacked.
-- | If a package isn't installed, `pacman -T` will yield a single name.
-- Any other type of output means installation is not required. 
mustInstall :: String -> Aura Bool
mustInstall pkg = ((==) 1 . length . words) `fmap` pacmanOutput ["-T",pkg]

conflicts :: (Package p, Buildable b) => (Settings -> p -> Maybe ErrMsg) ->
             Settings -> ([p],[b],[VirtualPkg]) -> [ErrMsg]
conflicts subConflict ss (subs,mains,virts) = sErr ++ mErr ++ vErr
    where sErr = mapMaybe (subConflict ss) subs
          mErr = mapMaybe (conflict ss) mains
          vErr = mapMaybe (conflict ss) virts
