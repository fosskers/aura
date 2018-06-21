{-# LANGUAGE OverloadedStrings, TupleSections #-}

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

module Aura.Packages.Repository
  ( pacmanRepo
  , extractVersion
  ) where

import           Aura.Core
import           Aura.Languages (provides_1)
import           Aura.Pacman (pacmanOutput)
import           Aura.Types
import           BasePrelude hiding (try)
import           Control.Concurrent.Async
import           Data.Bitraversable (bitraverse)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Versions
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utilities (getSelection)

---

-- | Repository package source.
-- We expect this to fail when the package is actually an AUR package.
pacmanRepo :: Repository
pacmanRepo = Repository $ \_ names -> do
  badsgoods <- partitionEithers <$> mapConcurrently resolveName (toList names)
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
resolveName :: T.Text -> IO (Either T.Text (T.Text, Provides))
resolveName name = do
  provs <- T.lines <$> pacmanOutput ["-Ssq", "^" <> name <> "$"]
  case provs of
    [] -> pure $ Left name
    _  -> Right . (, Provides name) <$> chooseProvider name provs

-- | Choose a providing package, favoring installed packages.
chooseProvider :: MonadIO m => T.Text -> [T.Text] -> m T.Text
chooseProvider name []  = pure name
chooseProvider _    [p] = pure p
chooseProvider name ps  = do
  mp <- listToMaybe . catMaybes <$> liftIO (mapConcurrently isInstalled ps)
  maybe f pure mp
  where f = liftIO $ warn (provides_1 name) *> getSelection ps

-- | The most recent version of a package, if it exists in the respositories.
mostRecentVersion :: T.Text -> IO (Maybe Versioning)
mostRecentVersion s = extractVersion <$> pacmanOutput ["-Si", s]

extractVersion :: T.Text -> Maybe Versioning
extractVersion = either (const Nothing) Just . parse p "extractVersion"
  where p = do
          takeWhile1P Nothing (/= '\n') *> newline
          takeWhile1P Nothing (/= '\n') *> newline
          string "Version" *> space1 *> char ':' *> space1 *> v
        v = choice [ try (fmap Ideal semver'    <* string "Description")
                   , try (fmap General version' <* string "Description")
                   , fmap Complex mess'         <* string "Description" ]
