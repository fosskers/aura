{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}

-- A library for working with the package cache.

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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
  , cacheMatches
  , pkgsInCache
  , Cache(..)
  , SimplePkg(..)
  ) where

import           Aura.Settings.Base
import           Aura.Types
import           BasePrelude hiding (FilePath)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly

---

newtype Cache = Cache { _cache :: M.Map SimplePkg PackagePath }

defaultPackageCache :: FilePath
defaultPackageCache = "/var/cache/pacman/pkg/"

-- TODO SILENT DROPS PATHS THAT DON'T PARSE
cache :: [PackagePath] -> Cache
cache = Cache . M.fromList . mapMaybe (\p -> (,p) <$> simplepkg p)

cacheContents :: FilePath -> Sh Cache
cacheContents = fmap (cache . map PackagePath) . lsT

pkgsInCache :: Settings -> [T.Text] -> Sh [T.Text]
pkgsInCache ss ps = do
  c <- cacheContents . fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  pure . nub . filter (`elem` ps) . map _spName . M.keys $ _cache c

cacheMatches :: Settings -> T.Text -> Sh [PackagePath]
cacheMatches ss input = do
  c <- cacheContents . fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  pure . filter (T.isInfixOf input . _pkgpath) . M.elems $ _cache c
