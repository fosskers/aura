{-# LANGUAGE OverloadedStrings #-}

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

module Aura.Types
  ( -- * Package Types
    SimplePkg(..), simplepkg, simplepkg'
    -- * Other Wrappers
  , PackagePath(..), sortPkgs
  ) where

import           BasePrelude hiding (try)
import           Data.Bitraversable
import qualified Data.Text as T
import           Data.Versions
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | A package name with its version number.
data SimplePkg = SimplePkg { _spName :: T.Text, _spVersion :: Versioning } deriving (Eq, Ord, Show)

-- | Attempt to create a `SimplePkg` from filepaths like:
--     linux-3.2.14-1-x86_64.pkg.tar.xz
simplepkg :: PackagePath -> Maybe SimplePkg
simplepkg (PackagePath t) = uncurry SimplePkg <$> bitraverse f f (parse n "name" t, parse v "version" t)
  where n :: Parsec Void T.Text T.Text
        n = T.pack <$> manyTill anyChar (try finished)

        -- | Assumes that a version number will never start with a letter,
        -- and that a package name section (i.e. abc-def-ghi) will never start
        -- with a number.
        finished = char '-' *> lookAhead digitChar
        v    = manyTill anyChar (try finished) *> ver
        ver  = try (fmap Ideal semver' <* post) <|> try (fmap General version' <* post) <|> fmap Complex mess'
        post = char '-' *> (string "x86_64" <|> string "any") *> string ".pkg.tar.xz"
        f    = either (const Nothing) Just

-- | Attempt to create a `SimplePkg` from text like:
--     xchat 2.8.8-19
simplepkg' :: T.Text -> Maybe SimplePkg
simplepkg' = either (const Nothing) Just . parse parser "name-and-version"
  where parser = SimplePkg <$> takeWhile1P Nothing (/= ' ') <*> (space *> versioning')

-- | Filepaths like:
--   * linux-3.2.14-1-x86_64.pkg.tar.xz
--   * wine-1.4rc6-1-x86_64.pkg.tar.xz
--   * ruby-1.9.3_p125-4-x86_64.pkg.tar.xz
newtype PackagePath = PackagePath { _pkgpath :: T.Text } deriving (Eq, Ord)

sortPkgs :: [PackagePath] -> [PackagePath]
sortPkgs = sortBy verNums
    where verNums a b | name a /= name b = compare a b  -- Different pkgs
                      | otherwise        = compare (ver a) (ver b)
          name = fmap _spName . simplepkg
          ver  = fmap _spVersion . simplepkg
