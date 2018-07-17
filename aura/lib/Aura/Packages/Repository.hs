{-# LANGUAGE OverloadedStrings, TupleSections #-}

-- |
-- Module    : Aura.Packages.Repository
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle the testing and dependency solving of official repository packages.

module Aura.Packages.Repository
  ( pacmanRepo
  , extractVersion
  ) where

import           Aura.Core
import           Aura.Languages (provides_1)
import           Aura.Pacman (pacmanOutput)
import           Aura.Settings (Settings)
import           Aura.Types
import           Aura.Utils (getSelection)
import           BasePrelude hiding (try)
import           Control.Concurrent.Async
import           Data.Bitraversable (bitraverse)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Versions
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Repository package source.
-- We expect this to fail when the package is actually an AUR package.
pacmanRepo :: Repository
pacmanRepo = Repository $ \ss names -> do
  badsgoods <- partitionEithers <$> mapConcurrently (resolveName ss) (toList names)
  bitraverse (pure . S.fromList) (fmap S.fromList . traverse f) badsgoods
  where f (r, p) = packageRepo r p <$> mostRecentVersion r

packageRepo :: PkgName -> Provides -> Maybe Versioning -> Package
packageRepo name pro ver = Package
  { _pkgName        = name
  , _pkgVersion     = ver
  , _pkgBaseName    = name
  , _pkgProvides    = pro
  , _pkgDeps        = []  -- Let pacman handle dependencies.
  , _pkgInstallType = Pacman name }

-- | If given a virtual package, try to find a real package to install.
-- Functions like this are why we need libalpm.
resolveName :: Settings -> PkgName -> IO (Either PkgName (PkgName, Provides))
resolveName ss name = do
  provs <- map PkgName . T.lines <$> pacmanOutput ["-Ssq", "^" <> _pkgname name <> "$"]
  case provs of
    [] -> pure $ Left name
    _  -> Right . (, Provides $ _pkgname name) <$> chooseProvider ss name provs

-- | Choose a providing package, favoring installed packages.
chooseProvider :: Settings -> PkgName -> [PkgName] -> IO PkgName
chooseProvider _ name []         = pure name
chooseProvider _ _    [p]        = pure p
chooseProvider ss name ps@(a:as) =
  mapConcurrently isInstalled ps >>= maybe f pure . listToMaybe . catMaybes
  where f = do
          warn ss $ provides_1 name
          PkgName <$> getSelection (_pkgname a :| map _pkgname as)

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: PkgName -> IO (Maybe Versioning)
mostRecentVersion (PkgName s) = extractVersion <$> pacmanOutput ["-Si", s]

-- | Parses the version number of a package from the result of a
-- @pacman -Si@ call.
extractVersion :: T.Text -> Maybe Versioning
extractVersion = either (const Nothing) Just . parse p "extractVersion"
  where p = do
          takeWhile1P Nothing (/= '\n') *> newline
          takeWhile1P Nothing (/= '\n') *> newline
          string "Version" *> space1 *> char ':' *> space1 *> v
        v = choice [ try (fmap Ideal semver'    <* string "Description")
                   , try (fmap General version' <* string "Description")
                   , fmap Complex mess'         <* string "Description" ]
