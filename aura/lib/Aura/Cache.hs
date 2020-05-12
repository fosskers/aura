{-# LANGUAGE TupleSections #-}

-- |
-- Module    : Aura.Cache
-- Copyright : (c) Colin Woodbury, 2012 - 2020
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
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T

---

-- | Every package in the current cache, paired with its original filename.
newtype Cache = Cache { _cache :: Map SimplePkg PackagePath }

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
cacheContents :: FilePath -> IO Cache
cacheContents pth = cache . mapMaybe (packagePath . (pth </>)) <$> listDirectory pth

-- | All packages from a given `Set` who have a copy in the cache.
pkgsInCache :: Settings -> Set PkgName -> IO (Set PkgName)
pkgsInCache ss ps = do
  c <- cacheContents . either id id . cachePathOf $ commonConfigOf ss
  pure . S.filter (`S.member` ps) . S.map spName . M.keysSet $ _cache c

-- | Any entries (filepaths) in the cache that match a given `Text`.
cacheMatches :: Settings -> Text -> IO [PackagePath]
cacheMatches ss input = do
  c <- cacheContents . either id id . cachePathOf $ commonConfigOf ss
  pure . filter (T.isInfixOf input . T.pack . ppPath) . M.elems $ _cache c
