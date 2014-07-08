-- A library for working with the package cache.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Cache
    ( defaultPackageCache
    , cacheContents
    , cacheFilter
    , cacheMatches
    , pkgsInCache
    , alterable
    , getFilename
    , allFilenames
    , size
    , Cache
    , SimplePkg ) where

import qualified Data.Map.Lazy as M
import Data.List (nub)

import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Utils (pkgFileNameAndVer)

import Utilities (searchLines)
import Shell     (ls)

---

type SimplePkg = (String,[Int])
type Cache     = M.Map SimplePkg FilePath

defaultPackageCache :: FilePath
defaultPackageCache = "/var/cache/pacman/pkg/"

simplePkg :: FilePath -> SimplePkg
simplePkg = pkgFileNameAndVer

cache :: [FilePath] -> Cache
cache = M.fromList . map pair
    where pair p = (simplePkg p, p)

-- This takes the filepath of the package cache as an argument.
rawCacheContents :: FilePath -> Aura [String]
rawCacheContents c = filter dots <$> liftIO (ls c)
    where dots p = p `notElem` [".",".."]

cacheContents :: FilePath -> Aura Cache
cacheContents c = cache <$> rawCacheContents c

pkgsInCache :: [String] -> Aura [String]
pkgsInCache ps =
    nub . filter (`elem` ps) . allNames <$> (asks cachePathOf >>= cacheContents)

cacheMatches :: [String] -> Aura [String]
cacheMatches input = asks cachePathOf >>= cacheContents >>=
                     return . searchLines (unwords input) . allFilenames

alterable :: Cache -> SimplePkg -> Bool
alterable c p = M.member p c

getFilename :: Cache -> SimplePkg -> Maybe FilePath
getFilename c p = M.lookup p c

allNames :: Cache -> [String]
allNames = map fst . M.keys

allFilenames :: Cache -> [FilePath]
allFilenames = M.elems

size :: Cache -> Int
size = M.size

cacheFilter :: (SimplePkg -> FilePath -> Bool) -> Cache -> Cache
cacheFilter = M.filterWithKey
