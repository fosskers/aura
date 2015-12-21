{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

import Data.Either      (partitionEithers)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Aura.Settings.Base
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Utils

import Aura.Shell
import Shelly hiding (find, liftIO)
import Prelude hiding (FilePath, span)
import Utilities (exists)

---

--------
-- TYPES
--------
type Error    = T.Text
type Pkgbuild = T.Text

data VersionDemand = LessThan T.Text
                   | AtLeast T.Text
                   | MoreThan T.Text
                   | MustBe T.Text
                   | Anything
                     deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = show ("<" <> v)
    show (AtLeast v)  = show (">=" <> v)
    show (MoreThan v) = show (">" <> v)
    show (MustBe  v)  = show ("=" <> v)
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
    { baseNameOf   :: T.Text
    , pkgbuildOf   :: Pkgbuild
    -- | Did the user select this package, or is it being built as a dep?
    , isExplicit   :: Bool
    -- | Fetch and extract the source code corresponding to the given package.
    , buildScripts :: FilePath     -- ^ Directory in which to place the scripts.
                   -> Aura (Maybe FilePath)  -- ^ Path to the extracted scripts.
    }

-- | A 'Repository' is a place where packages may be fetched from. Multiple
-- repositories can be combined with the 'Data.Monoid' instance.
newtype Repository = Repository
    { repoLookup :: T.Text -> Aura (Maybe Package) }

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

parseDep :: T.Text -> Dep
parseDep s = Dep name (getVersionDemand comp ver)
    where (name, comp, ver) = splitNameAndVer s
          getVersionDemand c v | c == Just "<"  = LessThan v
                               | c == Just ">=" = AtLeast v
                               | c == Just ">"  = MoreThan v
                               | c == Just "="  = MustBe v
                               | otherwise = Anything

-----------
-- THE WORK
-----------
-- | Action won't be allowed unless user is root, or using sudo.
sudo :: Aura () -> Aura ()
sudo action = do
  hasPerms <- liftShelly hasRootPriv
  if hasPerms then action else scoldAndFail mustBeRoot_1

-- | Stop the user if they are the true Root. Building as isn't allowed
-- as of makepkg v4.2.
trueRoot :: Aura () -> Aura ()
trueRoot action = ask >>= \ss -> do
  rootp <- liftShelly isntTrueRoot
  if rootp || buildUserOf ss /= "root"
    then action else scoldAndFail trueRoot_3

-- `-Qm` yields a list of sorted values.
-- | A list of non-prebuilt packages installed on the system.
foreignPackages :: Aura [(T.Text, T.Text)]
foreignPackages = (fmap fixName . T.lines) <$> pacmanOutput ["-Qm"]
    where fixName = (\ (name, ver) -> (name, T.stripStart ver)) . T.breakOn " "

orphans :: Aura [T.Text]
orphans = T.lines <$> pacmanOutput ["-Qqdt"]

develPkgs :: Aura [T.Text]
develPkgs = (filter isDevelPkg . fmap fst) <$> foreignPackages

isDevelPkg :: T.Text -> Bool
isDevelPkg p = any (`T.isSuffixOf` p) suffixes
    where suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- This could be:
-- isIgnored :: String -> Aura Bool
-- isIgnored pkg = asks (elem pkg . ignoredPkgsOf)
isIgnored :: T.Text -> [T.Text] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: T.Text -> Aura Bool
isInstalled pkg = pacmanSuccess ["-Qq", pkg]

removePkgs :: [T.Text] -> [T.Text] -> Aura ()
removePkgs [] _         = pure ()
removePkgs pkgs pacOpts = pacman  $ ["-Rsu"] <> pkgs <> pacOpts

-- Moving to a libalpm backend will make this less hacked.
-- | True if a dependency is satisfied by an installed package.
isSatisfied :: Dep -> Aura Bool
isSatisfied (Dep name ver) = T.null <$> pacmanOutput ["-T", name <> T.pack (show ver)]

-- | Block further action until the database is free.
checkDBLock :: Aura ()
checkDBLock = do
  locked <- liftShelly $ exists lockFile
  when locked $ warn checkDBLock_1 *> liftIO IO.getLine *> checkDBLock

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------
colouredMessage :: Colouror -> (Language -> T.Text) -> Aura ()
colouredMessage c msg = ask >>= putStrLnA c . msg . langOf

renderColour :: Colouror -> (Language -> T.Text) -> Aura T.Text
renderColour c msg = asks (c . msg . langOf)

say :: (Language -> T.Text) -> Aura ()
say = colouredMessage noColour

notify :: (Language -> T.Text) -> Aura ()
notify = colouredMessage green

warn :: (Language -> T.Text) -> Aura ()
warn = colouredMessage yellow

scold :: (Language -> T.Text) -> Aura ()
scold = colouredMessage red

badReport :: (Language -> T.Text) -> [T.Text] -> Aura ()
badReport _ []     = pure ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs
