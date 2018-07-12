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
  bitraverse (pure . S.fromList) (traverse f) badsgoods
  where f (r, p) = packageRepo r p <$> mostRecentVersion r

packageRepo :: T.Text -> Provides -> Maybe Versioning -> Package
packageRepo name pro ver = Package
  { pkgNameOf        = name
  , pkgVersionOf     = ver
  , pkgBaseNameOf    = name
  , pkgProvidesOf    = pro
  , pkgDepsOf        = []  -- Let pacman handle dependencies.
  , pkgInstallTypeOf = Pacman name }

-- | If given a virtual package, try to find a real package to install.
-- Functions like this are why we need libalpm.
resolveName :: Settings -> T.Text -> IO (Either T.Text (T.Text, Provides))
resolveName ss name = do
  provs <- T.lines <$> pacmanOutput ["-Ssq", "^" <> name <> "$"]
  case provs of
    [] -> pure $ Left name
    _  -> Right . (, Provides name) <$> chooseProvider ss name provs

-- | Choose a providing package, favoring installed packages.
chooseProvider :: Settings -> T.Text -> [T.Text] -> IO T.Text
chooseProvider _ name []         = pure name
chooseProvider _ _    [p]        = pure p
chooseProvider ss name ps@(a:as) = do
  mp <- listToMaybe . catMaybes <$> mapConcurrently isInstalled ps
  maybe f pure mp
  where f = warn ss (provides_1 name) *> getSelection (a :| as)

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: T.Text -> IO (Maybe Versioning)
mostRecentVersion s = extractVersion <$> pacmanOutput ["-Si", s]

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
