{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

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
    Package(..)
  , SimplePkg(..), simplepkg, simplepkg'
  , Dep(..), parseDep
  , Buildable(..)
    -- * Package Building
  , VersionDemand(..), _VersionDemand
  , InstallType(..)
    -- * Errors
  , DepError(..)
  , Failure(..)
    -- * Language
  , Language(..)
    -- * Other Wrappers
  , PackagePath(..), sortPkgs
  , Pkgbuild(..)
  , Provides(..)
  ) where

import           BasePrelude hiding (try)
import           Data.Bitraversable
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc hiding (space)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Versions
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- TODO Make all these fields strict, here and elsewhere.
-- | A package to be installed.
data Package = Package { pkgNameOf        :: T.Text
                       , pkgVersionOf     :: Maybe Versioning
                       , pkgBaseNameOf    :: T.Text
                       , pkgProvidesOf    :: Provides
                       , pkgDepsOf        :: [Dep]
                       , pkgInstallTypeOf :: InstallType }

-- | Hacky instance to allow `Package` to be used in a `Set`. Beware.
instance Eq Package where
  a == b = pkgNameOf a == pkgNameOf b && pkgVersionOf a == pkgVersionOf b

instance Ord Package where
  compare a b = case compare (pkgNameOf a) (pkgNameOf b) of
    EQ  -> compare (pkgVersionOf a) (pkgVersionOf b)
    oth -> oth
  -- a <= b = pkgNameOf a <= pkgNameOf b && pkgVersionOf a <= pkgVersionOf b

instance Show Package where
  show p = printf "%s (%s)" (show $ pkgNameOf p) (show . fmap prettyV $ pkgVersionOf p)

-- | A dependency on another package.
data Dep = Dep { depNameOf      :: T.Text
               , depVerDemandOf :: VersionDemand } deriving (Eq, Show)

-- TODO Return an Either if it failed to parse?
parseDep :: T.Text -> Maybe Dep
parseDep = either (const Nothing) Just . parse dep "dep"
  where dep :: Parsec Void T.Text Dep
        dep = Dep <$> takeWhile1P Nothing (\c -> c /= '<' && c /= '>' && c /= '=') <*> ver

        ver :: Parsec Void T.Text VersionDemand
        ver = do
          end <- atEnd
          if | end       -> pure Anything
             | otherwise -> choice [ char '<'    *> fmap LessThan versioning'
                                   , string ">=" *> fmap AtLeast  versioning'
                                   , char '>'    *> fmap MoreThan versioning'
                                   , char '='    *> fmap MustBe   versioning'
                                   , pure Anything ]

-- | The versioning requirement of some package's dependency.
data VersionDemand = LessThan Versioning
                   | AtLeast  Versioning
                   | MoreThan Versioning
                   | MustBe   Versioning
                   | Anything
                   deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = T.unpack $ "<"  <> prettyV v
    show (AtLeast  v) = T.unpack $ ">=" <> prettyV v
    show (MoreThan v) = T.unpack $ ">"  <> prettyV v
    show (MustBe   v) = T.unpack $ "="  <> prettyV v
    show Anything     = "Anything"

_VersionDemand :: Traversal' VersionDemand Versioning
_VersionDemand f (LessThan v) = LessThan <$> f v
_VersionDemand f (AtLeast v)  = AtLeast  <$> f v
_VersionDemand f (MoreThan v) = MoreThan <$> f v
_VersionDemand f (MustBe v)   = MustBe   <$> f v
_VersionDemand _ p            = pure p

-- | The installation method.
data InstallType = Pacman T.Text | Build Buildable

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

-- | The contents of a PKGBUILD file.
newtype Pkgbuild = Pkgbuild { _pkgbuild :: T.Text }

-- | The dependency which some package provides. May not be the same name
-- as the package itself (e.g. cronie provides cron).
newtype Provides = Provides { _provides :: T.Text }

-- | A package to be built manually before installing.
data Buildable = Buildable
    { bldNameOf     :: T.Text
    , bldBaseNameOf :: T.Text
    , bldProvidesOf :: Provides
    , pkgbuildOf    :: Pkgbuild
    , bldDepsOf     :: [Dep]
    , bldVersionOf  :: Maybe Versioning
    -- | Did the user select this package, or is it being built as a dep?
    , isExplicit    :: Bool
    -- | Fetch and extract the source code corresponding to the given package.
    -- Expects a directory in which to place the scripts, and yields the path
    -- they were successfully extracted to.
    , buildScripts  :: FilePath -> IO (Maybe FilePath) }

data Language = English
              | Japanese
              | Polish
              | Croatian
              | Swedish
              | German
              | Spanish
              | Portuguese
              | French
              | Russian
              | Italian
              | Serbian
              | Norwegian
              | Indonesia
              | Chinese
                deriving (Eq, Enum, Ord, Show)

data DepError = NonExistant T.Text
              | VerConflict (Doc AnsiStyle)
              | Ignored (Doc AnsiStyle)
              | UnparsableVersion T.Text
              | BrokenProvides T.Text T.Text T.Text

-- | Some failure message that when given the current runtime `Language`
-- will produce a human-friendly error.
newtype Failure = Failure { _failure :: Language -> Doc AnsiStyle }
