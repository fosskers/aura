{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
  , Cache
  , SimplePkg(..)
  ) where

import           Aura.Settings.Base
import           Aura.Utils (pkgFileNameAndVer)
import           Aura.Utils.Numbers (Version)
import           BasePrelude hiding (Version)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly

---

data SimplePkg = SimplePkg { _spName :: T.Text, _spVersion :: Maybe Version } deriving (Eq, Ord)
type Cache     = M.Map SimplePkg T.Text

defaultPackageCache :: T.Text
defaultPackageCache = "/var/cache/pacman/pkg/"

simplePkg :: T.Text -> SimplePkg
simplePkg = uncurry SimplePkg . first T.pack . pkgFileNameAndVer . T.unpack

cache :: [T.Text] -> Cache
cache = M.fromList . map (simplePkg &&& id)

cacheContents :: T.Text -> Sh Cache
cacheContents = fmap cache . lsT . fromText

pkgsInCache :: Settings -> [T.Text] -> Sh [T.Text]
pkgsInCache ss ps =
  nub . filter (`elem` ps) . map _spName . M.keys <$> cacheContents (cachePathOf ss)

cacheMatches :: Settings -> [T.Text] -> Sh [T.Text]
cacheMatches ss (T.unwords -> input) =
  filter (T.isInfixOf input) . M.elems <$> cacheContents (cachePathOf ss)
