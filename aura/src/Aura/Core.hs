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

module Aura.Core where

import           Aura.Colour.Text
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings.Base
import           Aura.Utils
import           BasePrelude
import qualified Data.Text as T
import           System.Directory (doesFileExist)
import           Text.Regex.PCRE ((=~))
import           Utilities

---

--------
-- TYPES
--------
type Error    = String
type Pkgbuild = String

data VersionDemand = LessThan String
                   | AtLeast String
                   | MoreThan String
                   | MustBe String
                   | Anything
                     deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = "<" <> v
    show (AtLeast v)  = ">=" <> v
    show (MoreThan v) = ">" <> v
    show (MustBe  v)  = "=" <> v
    show Anything     = ""

-- | A package to be installed.
data Package = Package { pkgNameOf        :: T.Text
                       , pkgVersionOf     :: T.Text
                       , pkgDepsOf        :: [Dep]
                       , pkgInstallTypeOf :: InstallType }

-- | A dependency on another package.
data Dep = Dep { depNameOf      :: T.Text
               , depVerDemandOf :: VersionDemand }

-- | The installation method.
data InstallType = Pacman T.Text | Build Buildable

-- | A package to be built manually before installing.
data Buildable = Buildable
    { baseNameOf   :: String
    , pkgbuildOf   :: Pkgbuild
    , bldDepsOf    :: [Dep]
    , bldVersionOf :: String
    -- | Did the user select this package, or is it being built as a dep?
    , isExplicit   :: Bool
    -- | Fetch and extract the source code corresponding to the given package.
    , buildScripts :: FilePath             -- ^ Directory in which to place the scripts.
                   -> IO (Maybe FilePath)  -- ^ Path to the extracted scripts.
    }

-- | A 'Repository' is a place where packages may be fetched from. Multiple
-- repositories can be combined with the 'Data.Monoid' instance.
newtype Repository = Repository { repoLookup :: T.Text -> Aura (Maybe Package) }

instance Monoid Repository where
  mempty = Repository $ const (pure Nothing)

  a `mappend` b = Repository $ \s -> do
    mpkg <- repoLookup a s
    case mpkg of
      Nothing -> repoLookup b s
      _       -> pure mpkg

---------------------------------
-- Functions common to `Package`s
---------------------------------
-- | Partition a list of packages into pacman and buildable groups.
partitionPkgs :: [Package] -> ([T.Text], [Buildable])
partitionPkgs = partitionEithers . fmap (toEither . pkgInstallTypeOf)
  where toEither (Pacman s) = Left  s
        toEither (Build  b) = Right b

parseDep :: String -> Dep
parseDep s = Dep (T.pack name) (getVersionDemand comp ver)
    where patt = "(<|>=|>|=)" :: String
          (name, comp, ver) = s =~ patt :: (String, String, String)
          getVersionDemand c v | c == "<"  = LessThan v
                               | c == ">=" = AtLeast v
                               | c == ">"  = MoreThan v
                               | c == "="  = MustBe v
                               | otherwise = Anything

-----------
-- THE WORK
-----------
-- | Action won't be allowed unless user is root, or using sudo.
sudo :: Aura () -> Aura ()
sudo action = do
  hasPerms <- asks (hasRootPriv . environmentOf)
  if hasPerms then action else scoldAndFail mustBeRoot_1

-- | Stop the user if they are the true Root. Building as isn't allowed
-- as of makepkg v4.2.
trueRoot :: Aura () -> Aura ()
trueRoot action = ask >>= \ss ->
  if not (isTrueRoot $ environmentOf ss) || buildUserOf ss /= User "root"
    then action else scoldAndFail trueRoot_3

-- `-Qm` yields a list of sorted values.
-- | A list of non-prebuilt packages installed on the system.
foreignPackages :: Aura [(T.Text, T.Text)]
foreignPackages = map fixName . T.lines <$> pacmanOutput ["-Qm"]
  where fixName = second T.tail . T.span (/= ' ')

orphans :: Aura [T.Text]
orphans = T.lines <$> pacmanOutput ["-Qqdt"]

develPkgs :: Aura [T.Text]
develPkgs = (filter isDevelPkg . map fst) <$> foreignPackages

isDevelPkg :: T.Text -> Bool
isDevelPkg pkg = any (\s -> T.isSuffixOf s pkg) suffixes
  where suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- This could be:
-- isIgnored :: String -> Aura Bool
-- isIgnored pkg = asks (elem pkg . ignoredPkgsOf)
-- isIgnored :: String -> [String] -> Bool
-- isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: T.Text -> Aura Bool
isInstalled pkg = pacmanSuccess ["-Qq", pkg]

removePkgs :: [T.Text] -> [T.Text] -> Aura ()
removePkgs [] _         = pure ()
removePkgs pkgs pacOpts = pacman $ ["-Rsu"] <> pkgs <> pacOpts

-- Moving to a libalpm backend will make this less hacked.
-- | True if a dependency is satisfied by an installed package.
isSatisfied :: Dep -> Aura Bool
isSatisfied (Dep name ver) = T.null <$> pacmanOutput ["-T", name <> T.pack (show ver)]

-- | Block further action until the database is free.
checkDBLock :: Aura ()
checkDBLock = do
  locked <- liftIO $ doesFileExist lockFile
  when locked $ (asks langOf >>= warn . checkDBLock_1) *> liftIO getLine *> checkDBLock

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------

renderColour :: Colouror -> (Language -> String) -> Aura String
renderColour c msg = asks (c . msg . langOf)

say :: MonadIO m => String -> m ()
say = putStrLnA noColour

notify :: MonadIO m => String -> m ()
notify = putStrLnA green

warn :: MonadIO m => String -> m ()
warn = putStrLnA yellow

scold :: MonadIO m => String -> m ()
scold = putStrLnA red

badReport :: (Language -> String) -> [String] -> Aura ()
badReport _ []     = pure ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs
