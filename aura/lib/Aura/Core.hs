{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}

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

module Aura.Core where

import           Aura.Colour.Text
import           Aura.Errors
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings.Base
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Versions
import           Shelly (Sh, test_f)
import           Text.Megaparsec hiding (failure)
import           Text.Megaparsec.Char
import           Utilities

---

--------
-- TYPES
--------

-- TODO Move all these types to the `Types` module

newtype Pkgbuild = Pkgbuild { _pkgbuild :: T.Text }

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
    show Anything     = ""

-- | A package to be installed.
data Package = Package { pkgNameOf        :: T.Text
                       , pkgVersionOf     :: T.Text
                       , pkgDepsOf        :: [Dep]
                       , pkgInstallTypeOf :: InstallType }

-- | A dependency on another package.
data Dep = Dep { depNameOf      :: T.Text
               , depVerDemandOf :: VersionDemand } deriving (Eq, Show)

-- | The installation method.
data InstallType = Pacman T.Text | Build Buildable

-- | A package to be built manually before installing.
data Buildable = Buildable
    { baseNameOf   :: T.Text
    , pkgbuildOf   :: Pkgbuild
    , bldDepsOf    :: [Dep]
    , bldVersionOf :: T.Text
    -- | Did the user select this package, or is it being built as a dep?
    , isExplicit   :: Bool
    -- | Fetch and extract the source code corresponding to the given package.
    , buildScripts :: FilePath             -- ^ Directory in which to place the scripts.
                   -> IO (Maybe FilePath)  -- ^ Path to the extracted scripts.
    }

-- | A 'Repository' is a place where packages may be fetched from. Multiple
-- repositories can be combined with the 'Data.Monoid' instance.
newtype Repository = Repository { repoLookup :: forall m. MonadIO m => Settings -> T.Text -> m (Maybe Package) }

instance Semigroup Repository where
  a <> b = Repository $ \ss p -> do
    mpkg <- repoLookup a ss p
    case mpkg of
      Nothing -> repoLookup b ss p
      _       -> pure mpkg

-- TODO Make the version field a proper `Versioning`
-- | A package installed from the AUR.
data ForeignPkg = ForeignPkg { _fNameOf :: !T.Text, _fVerOf :: !T.Text }

---------------------------------
-- Functions common to `Package`s
---------------------------------
-- | Partition a list of packages into pacman and buildable groups.
partitionPkgs :: [Package] -> ([T.Text], [Buildable])
partitionPkgs = partitionEithers . fmap (toEither . pkgInstallTypeOf)
  where toEither (Pacman s) = Left  s
        toEither (Build  b) = Right b

-- | Return an Either if it failed to parse?
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

-----------
-- THE WORK
-----------
-- | Action won't be allowed unless user is root, or using sudo.
sudo :: Aura a -> Aura (Either Failure a)
sudo action =
  asks (hasRootPriv . envOf) >>= bool (pure $ failure mustBeRoot_1) (Right <$> action)

-- | Stop the user if they are the true Root. Building as isn't allowed
-- as of makepkg v4.2.
trueRoot :: Aura a -> Aura (Either Failure a)
trueRoot action = ask >>= \ss ->
  if not (isTrueRoot $ envOf ss) || buildUserOf ss /= User "root"
    then Right <$> action else pure $ failure trueRoot_3

-- | A list of non-prebuilt packages installed on the system.
-- `-Qm` yields a list of sorted values.
foreignPackages :: Aura [ForeignPkg]
foreignPackages = map (uncurry ForeignPkg . fixName) . T.lines <$> pacmanOutput ["-Qm"]
  where fixName = second T.tail . T.span (/= ' ')

orphans :: Aura [T.Text]
orphans = T.lines <$> pacmanOutput ["-Qqdt"]

develPkgs :: Aura [T.Text]
develPkgs = filter isDevelPkg . map _fNameOf <$> foreignPackages
  where isDevelPkg pkg = any (`T.isSuffixOf` pkg) suffixes
        suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- | Returns what it was given if the package is already installed.
-- Reasoning: Using raw bools can be less expressive.
isInstalled :: MonadIO m => T.Text -> m (Maybe T.Text)
isInstalled pkg = bool Nothing (Just pkg) <$> pacmanSuccess ["-Qq", pkg]

removePkgs :: [T.Text] -> [T.Text] -> Aura (Either Failure ())
removePkgs [] _         = pure $ Right ()
removePkgs pkgs pacOpts = pacman $ ["-Rsu"] <> pkgs <> pacOpts

-- | True if a dependency is satisfied by an installed package.
isSatisfied :: MonadIO m => Dep -> m Bool
isSatisfied (Dep name ver) = T.null <$> pacmanOutput ["-T", name <> T.pack (show ver)]

-- | Block further action until the database is free.
checkDBLock :: Settings -> Sh ()
checkDBLock ss = do
  locked <- test_f lockFile
  when locked $ (warn . checkDBLock_1 $ langOf ss) *> liftIO getLine *> checkDBLock ss

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------

renderColour :: Colouror -> (Language -> String) -> Aura String
renderColour c msg = asks (c . msg . langOf)

notify :: MonadIO m => String -> m ()
notify = putStrLnA green

warn :: MonadIO m => String -> m ()
warn = putStrLnA yellow

scold :: MonadIO m => String -> m ()
scold = putStrLnA red

badReport :: (Language -> String) -> [String] -> Aura ()
badReport _ []     = pure ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs
