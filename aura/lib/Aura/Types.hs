{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

-- |
-- Module    : Aura.Types
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Core Aura types.

module Aura.Types
  ( -- * Package Types
    Package(..), pname, pprov, pver
  , Dep(..), parseDep
  , Buildable(..)
  , Prebuilt(..)
  , SimplePkg(..), simplepkg, simplepkg'
    -- * Typeclasses
  , Flagable(..)
    -- * Package Building
  , VersionDemand(..), _VersionDemand
  , InstallType(..)
    -- * Errors
  , DepError(..)
  , Failure(..)
    -- * Language
  , Language(..)
    -- * Other Wrappers
  , PkgName(..)
  , PkgGroup(..)
  , Provides(..)
  , PackagePath(..)
  , Pkgbuild(..)
  , Environment
  , User(..)
  ) where

import           BasePrelude hiding (try)
import           Control.Error.Util (hush)
import           Data.Aeson (FromJSONKey, ToJSONKey)
import           Data.Bitraversable
import qualified Data.ByteString.Lazy as BL
import           Data.Generics.Product (field, super)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc hiding (list, space)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Versions hiding (Traversal')
import           GHC.Generics (Generic)
import           Lens.Micro
import           System.Path (Absolute, Path, takeFileName, toUnrootedFilePath)
import           Text.Megaparsec
import           Text.Megaparsec.Char

---

-- | Types whose members can be converted to CLI flags.
class Flagable a where
  asFlag :: a -> [String]

instance Flagable T.Text where
  asFlag t = [T.unpack t]

instance (Foldable f, Flagable a) => Flagable (f a) where
  asFlag = foldMap asFlag

-- | A package to be installed.
data Package = FromRepo Prebuilt | FromAUR Buildable deriving (Eq)

-- | The name of a `Package`.
pname :: Package -> PkgName
pname (FromRepo pb) = pb ^. field @"name"
pname (FromAUR b)   = b  ^. field @"name"

-- | Other names which allow this `Package` to be satisfied as a dependency.
pprov :: Package -> Provides
pprov (FromRepo pb) = pb ^. field @"provides"
pprov (FromAUR b)   = b  ^. field @"provides"

-- | The version of a `Package`.
pver :: Package -> Versioning
pver (FromRepo pb) = pb ^. field @"version"
pver (FromAUR b)   = b  ^. field @"version"

-- TODO Figure out how to do this more generically.
instance Ord Package where
  compare (FromAUR a) (FromAUR b)   = compare a b
  compare (FromRepo a) (FromRepo b) = compare a b
  compare (FromAUR a) (FromRepo b)  = compare (a ^. super @SimplePkg) (b ^. super @SimplePkg)
  compare (FromRepo a) (FromAUR b)  = compare (a ^. super @SimplePkg) (b ^. super @SimplePkg)

-- | A `Package` from the AUR that's buildable in some way on the user's machine.
data Buildable = Buildable { name       :: !PkgName
                           , version    :: !Versioning
                           , base       :: !PkgName
                           , provides   :: !Provides
                           , deps       :: ![Dep]
                           , pkgbuild   :: !Pkgbuild
                           , isExplicit :: !Bool } deriving (Eq, Ord, Show, Generic)

-- | A prebuilt `Package` from the official Arch repositories.
data Prebuilt = Prebuilt { name     :: !PkgName
                         , version  :: !Versioning
                         , base     :: !PkgName
                         , provides :: !Provides } deriving (Eq, Ord, Show, Generic)

-- | A dependency on another package.
data Dep = Dep { name   :: !PkgName
               , demand :: !VersionDemand } deriving (Eq, Ord, Show, Generic)

-- | Parse a dependency entry as it would appear in a PKGBUILD:
--
-- @
-- >>> parseDep "pacman>1.2.3"
-- Just (Dep {name = PkgName {name = "pacman"}, demand = >1.2.3})
-- @
parseDep :: T.Text -> Maybe Dep
parseDep = hush . parse dep "dep"
  where dep = Dep <$> n <*> v
        n   = PkgName <$> takeWhile1P Nothing (\c -> c /= '<' && c /= '>' && c /= '=')
        v   = do
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
                   deriving (Eq, Ord)

instance Show VersionDemand where
    show (LessThan v) = T.unpack $ "<"  <> prettyV v
    show (AtLeast  v) = T.unpack $ ">=" <> prettyV v
    show (MoreThan v) = T.unpack $ ">"  <> prettyV v
    show (MustBe   v) = T.unpack $ "="  <> prettyV v
    show Anything     = "Anything"

-- | Attempt to zoom into the `Versioning` hiding within a `VersionDemand`.
_VersionDemand :: Traversal' VersionDemand Versioning
_VersionDemand f (LessThan v) = LessThan <$> f v
_VersionDemand f (AtLeast v)  = AtLeast  <$> f v
_VersionDemand f (MoreThan v) = MoreThan <$> f v
_VersionDemand f (MustBe v)   = MustBe   <$> f v
_VersionDemand _ p            = pure p

-- | The installation method.
data InstallType = Pacman PkgName | Build Buildable deriving (Eq)

-- | A package name with its version number.
data SimplePkg = SimplePkg { name :: !PkgName, version :: !Versioning } deriving (Eq, Ord, Show, Generic)

-- | Attempt to create a `SimplePkg` from filepaths like
--   @\/var\/cache\/pacman\/pkg\/linux-3.2.14-1-x86_64.pkg.tar.xz@
simplepkg :: PackagePath -> Maybe SimplePkg
simplepkg (PackagePath t) = uncurry SimplePkg <$> bitraverse hush hush (parse n "name" t', parse v "version" t')
  where t' = T.pack . toUnrootedFilePath $ takeFileName t

        n :: Parsec Void T.Text PkgName
        n = PkgName . T.pack <$> manyTill anySingle (try finished)

        -- | Assumes that a version number will never start with a letter,
        -- and that a package name section (i.e. abc-def-ghi) will never start
        -- with a number.
        finished = char '-' *> lookAhead digitChar
        v    = manyTill anySingle (try finished) *> ver
        ver  = try (fmap Ideal semver' <* post) <|> try (fmap General version' <* post) <|> fmap Complex mess'
        post = char '-' *> (string "x86_64" <|> string "any") *> string ".pkg.tar.xz"

-- | Attempt to create a `SimplePkg` from text like:
--     xchat 2.8.8-19
simplepkg' :: T.Text -> Maybe SimplePkg
simplepkg' = hush . parse parser "name-and-version"
  where parser = SimplePkg <$> (PkgName <$> takeWhile1P Nothing (/= ' ')) <*> (space *> versioning')

-- | Filepaths like:
--
--   * \/var\/cache\/pacman\/pkg\/linux-3.2.14-1-x86_64.pkg.tar.xz
--   * \/var\/cache\/pacman\/pkg\/wine-1.4rc6-1-x86_64.pkg.tar.xz
--   * \/var\/cache\/pacman\/pkg\/ruby-1.9.3_p125-4-x86_64.pkg.tar.xz
newtype PackagePath = PackagePath { path :: Path Absolute } deriving (Eq, Generic)

-- | If they have the same package names, compare by their versions.
-- Otherwise, do raw comparison of the path string.
instance Ord PackagePath where
  compare a b | nameA /= nameB = compare (path a) (path b)
              | otherwise      = compare verA verB
    where (nameA, verA) = f a
          (nameB, verB) = f b
          f = ((^? _Just . field @"name") &&& (^? _Just . field @"version")) . simplepkg

-- | The contents of a PKGBUILD file.
newtype Pkgbuild = Pkgbuild { pkgbuild :: BL.ByteString } deriving (Eq, Ord, Show, Generic)

-- | All human languages available for text output.
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
              | Esperanto
                deriving (Eq, Enum, Bounded, Ord, Show)

-- | The various ways that dependency resolution can fail.
data DepError = NonExistant PkgName
              | VerConflict (Doc AnsiStyle)
              | Ignored (Doc AnsiStyle)
              | BrokenProvides PkgName Provides PkgName

-- | Some failure message that when given the current runtime `Language`
-- will produce a human-friendly error.
newtype Failure = Failure { failure :: Language -> Doc AnsiStyle }

-- | Shell environment variables.
type Environment = M.Map T.Text T.Text

-- | The name of a user account on a Linux system.
newtype User = User { user :: T.Text } deriving (Eq, Show, Generic)

-- | The name of an Arch Linux package.
newtype PkgName = PkgName { name :: T.Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Flagable, ToJSONKey, FromJSONKey, IsString)

-- | A group that a `Package` could belong too, like @base@, @base-devel@, etc.
newtype PkgGroup = PkgGroup { group :: T.Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Flagable)

-- | The dependency which some package provides. May not be the same name
-- as the package itself (e.g. cronie provides cron).
newtype Provides = Provides { provides :: T.Text } deriving (Eq, Ord, Show, Generic)
