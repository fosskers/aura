{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module    : Aura.Cache
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Reading and searching the package cache.

module Aura.Cache
  ( -- * Types
    Cache(..)
  , cacheContents
    -- * Misc.
  , defaultPackageCache
  , cacheMatches
  , pkgsInCache
  ) where

import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (FilePath)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Shelly


---

-- | Every package in the current cache, paired with its original filename.
newtype Cache = Cache { _cache :: M.Map SimplePkg PackagePath }

-- | The default location of the package cache: \/var\/cache\/pacman\/pkg\/
defaultPackageCache :: FilePath
defaultPackageCache = "/var/cache/pacman/pkg/"

-- SILENT DROPS PATHS THAT DON'T PARSE
-- Maybe that's okay, since we don't know what non-package garbage files
-- could be sitting in the cache.
-- | Given every filepath contained in the package cache, form
-- the `Cache` type.
cache :: [PackagePath] -> Cache
cache = Cache . M.fromList . mapMaybe (\p -> (,p) <$> simplepkg p)

-- | Given a path to the package cache, yields its contents in a usable form.
cacheContents :: FilePath -> Sh Cache
cacheContents = fmap (cache . map PackagePath) . ls

-- | All packages from a given `S.Set` who have a copy in the cache.
pkgsInCache :: Settings -> NonEmptySet T.Text -> Sh (S.Set T.Text)
pkgsInCache ss ps = do
  c <- cacheContents . either id id . cachePathOf $ commonConfigOf ss
  pure . S.filter (`NES.member` ps) . S.map _spName . M.keysSet $ _cache c

-- | Any entries (filepaths) in the cache that match a given `T.Text`.
cacheMatches :: Settings -> T.Text -> Sh [PackagePath]
cacheMatches ss input = do
  c <- cacheContents . either id id . cachePathOf $ commonConfigOf ss
  pure . filter (T.isInfixOf input . toTextIgnore . _pkgpath) . M.elems $ _cache c
