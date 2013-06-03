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

import Control.Monad   (filterM)
import Data.Maybe      (fromJust, mapMaybe)
import Data.List       ((\\))

import Aura.Pacman        (pacmanOutput)
import Aura.Packages.Repository
import Aura.Packages.Virtual
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Graph
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities (notNull, alNotElem)

---

{- NOTE

`Main Packages` are either AUR or ABS packages.
`Sub Packages`  are either ABS or Repository packages.

These can (or should) freely overlap.

-}

data PkgStatus = Main | Dep deriving (Eq,Show)

type DepMap a = [(String,a)]

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
              -> Aura ([b],[p])
depsToInstall _ _ [] = ask >>= failure . getDepsToInstall_1 . langOf
depsToInstall subConflict bh pkgs = ask >>= \ss -> do
  (mains,subs,virts) <- depCheck bh pkgs
  subPkgs  <- filterM (mustInstall . show) subs
  mainPkgs <- filterM (depFilter pkgs) mains
  virPkgs  <- filterM mustInstall virts >>= packages
  let flicts = conflicts subConflict ss (subPkgs,mainPkgs,virPkgs)
  if notNull flicts
     then failure $ unlines flicts
     else do
       let providers = map (pkgNameOf . fromJust . providerPkgOf) virPkgs
       providers' <- packages $ (providers \\ map pkgNameOf subPkgs)
       return (mainPkgs, providers' ++ subPkgs)

-- | The packages originally asked to install for should always pass.
depFilter :: Buildable b => [b] -> b -> Aura Bool
depFilter ps b = if b `elem` ps then return True else mustInstall $ show b

depCheck :: (Package p, Buildable b)
         => BuildHandle -> [b] -> Aura ([b],[p],[String])
depCheck bh ps = depCheck' bh ps (graph [], [], [])

-- Recall that using `show` on a Package gives a String in the form:
-- `name>=version`  where `>=` could really be any of the comparison operators.
-- Virtual package names taken from PKGBUILDs should already be in this form,
-- so they need not be `show`n.
depCheck' :: (Package p, Buildable b)
          => BuildHandle -> [b] -> (DepGraph b, DepMap p, DepMap String)
          -> Aura ([b],[p],[String])
depCheck' _ [] (g,s,v)              = do
  liftIO $ print $ raw g
  liftIO $ print $ allNodes g
  return (allNodes g, map snd s, map snd v)
depCheck' bh (p:ps) (a@(_,_,k),s,v) = do  -- `p` will never be in the Graph.
  let ns    = namespaceOf p
      df dm = filter (\d -> alNotElem d dm)
      deps  = concatMap (value ns) ["depends","makedepends","checkdepends"]
      deps' = filter (not . k . fst . pnvd) . df v . df s $ deps
  (subNames,mainNames,other) <- divideByPkgType (subPF bh) (mainPF bh) deps'
  let mainNames' = filter (`notElem` map show ps) mainNames
  mainPkgs <- packages mainNames'
  subPkgs  <- packages subNames
  depCheck' bh (ps ++ mainPkgs) ( graphAdd (p,pkgNameOf p,map (fst . pnvd) deps) a
                                , i show s subPkgs
                                , i id v other )
      where i f  = foldr (\x acc -> ((f x),x) : acc)
            pnvd = parseNameAndVersionDemand

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
