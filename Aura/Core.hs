{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

import System.Directory (doesFileExist)
import Text.Regex.PCRE  ((=~))
import Control.Monad    (liftM,when)
import Data.List        (intercalate, isSuffixOf)

import Aura.Settings.Base
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Utils

import Bash.Base

import Utilities
import Shell

---

--------
-- TYPES
--------
type ErrMsg = String
type Pkgbuild = String
type PkgFilter = [String] -> Aura [String]

-----------
-- PACKAGES
-----------
class Package a where
    pkg :: String -> Aura a
    pkgNameOf :: a -> String
    versionOf :: a -> VersionDemand

data VersionDemand = LessThan String
                   | AtLeast String
                   | MoreThan String
                   | MustBe String
                   | Anything
                     deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = '<' : v
    show (AtLeast v)  = ">=" ++ v
    show (MoreThan v) = '>' : v
    show (MustBe  v)  = '=' : v
    show Anything     = ""

---------------
-- Source Packages
---------------

class (Package a, Show a, Eq a) => SourcePackage a where
  -- | Fetch and extract the source code corresponding to the given package.
  getSource :: a -- ^ Package (currently AUR or ABS)
            -> FilePath -- ^ Directory in which to extract the package.
            -> IO FilePath -- ^ Path to the extracted source.

  -- | Read the PKGBUILD of the package as a string.
  pkgbuildOf :: a -- ^ Package.
             -> Pkgbuild -- ^ PKGBUILD read in as a string.

  namespaceOf :: a -- ^ Package
              -> Namespace -- ^ Parsed key/value pairs from the PKGBUILD.

  -- | Parse a PKGBUILD file to create a SourcePackage.
  parsePkgbuild :: String -- ^ PKGBUILD location on disk
                -> Pkgbuild -- ^ PKGBUILD contents
                -> Aura a

------------------
-- Pacman Packages
------------------
data PacmanPkg = PacmanPkg String VersionDemand String

instance Package PacmanPkg where
    pkg = pacmanPkg
    pkgNameOf (PacmanPkg n _ _) = n
    versionOf (PacmanPkg _ v _) = v

instance Show PacmanPkg where
    show = pkgNameWithVersionDemand

instance Eq PacmanPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

pkgInfoOf :: PacmanPkg -> String
pkgInfoOf (PacmanPkg _ _ i) = i

pacmanPkg :: String -> Aura PacmanPkg
pacmanPkg pkg = PacmanPkg name ver `liftM` pacmanOutput ["-Si",name]
    where (name,ver) = parseNameAndVersionDemand pkg

-- | Get only those packages that are accessible by pacman.
filterRepoPkgs :: PkgFilter
filterRepoPkgs pkgs = do
  repoPkgs <- lines `liftM` pacmanOutput ["-Ssq",pkgs']
  return $ filter (`elem` repoPkgs) pkgs
    where pkgs' = "^(" ++ prep pkgs ++ ")$"
          prep  = specs . intercalate "|"
          specs []     = []
          specs (c:cs) | c `elem` "+" = ['[',c,']'] ++ specs cs
                       | otherwise    = c : specs cs

---------------------------------
-- Functions common to `Package`s
---------------------------------
pkgNameWithVersionDemand :: Package a => a -> String
pkgNameWithVersionDemand pkg = pkgNameOf pkg ++ signAndVersion
    where signAndVersion = show $ versionOf pkg

parseNameAndVersionDemand :: String -> (String,VersionDemand)
parseNameAndVersionDemand pkg = (name, getVersionDemand comp ver)
    where (name,comp,ver) = pkg =~ "(<|>=|>|=)" :: (String,String,String)
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
  hasPerms <- (hasRootPriv . environmentOf) `liftM` ask
  if hasPerms then action else scoldAndFail mustBeRoot_1

-- | Prompt if the user is the true Root. Building as it can be dangerous.
trueRoot :: Aura () -> Aura ()
trueRoot action = ask >>= \ss ->
  if isntTrueRoot $ environmentOf ss then action else do
       okay <- optionalPrompt trueRoot_1
       if okay then action else notify trueRoot_2

-- `-Qm` yields a list of sorted values.
getForeignPackages :: Aura [(String,String)]
getForeignPackages = (map fixName . lines) `liftM` pacmanOutput ["-Qm"]
    where fixName = hardBreak (== ' ')

getOrphans :: Aura [String]
getOrphans = lines `liftM` pacmanOutput ["-Qqdt"]

getDevelPkgs :: Aura [String]
getDevelPkgs = (filter isDevelPkg . map fst) `liftM` getForeignPackages

isDevelPkg :: String -> Bool
isDevelPkg p = any (`isSuffixOf` p) suffixes
    where suffixes = ["-git","-hg","-svn","-darcs","-cvs","-bzr"]

isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> Aura Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

removePkgs :: [String] -> [String] -> Aura ()
removePkgs [] _         = return ()
removePkgs pkgs pacOpts = pacman  $ ["-Rsu"] ++ pkgs ++ pacOpts

-- | Block further action until the database is free.
checkDBLock :: Aura ()
checkDBLock = do
  locked <- liftIO $ doesFileExist lockFile
  when locked $ warn checkDBLock_1 >> liftIO getLine >> checkDBLock

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------
colouredMessage :: Colouror -> (Language -> String) -> Aura ()
colouredMessage c msg = ask >>= putStrLnA c . msg . langOf

renderColour :: Colouror -> (Language -> String) -> Aura String
renderColour c msg = (c . msg . langOf) `liftM` ask

say :: (Language -> String) -> Aura ()
say = colouredMessage noColour

notify :: (Language -> String) -> Aura ()
notify = colouredMessage green

warn :: (Language -> String) -> Aura ()
warn = colouredMessage yellow

scold :: (Language -> String) -> Aura ()
scold = colouredMessage red

badReport :: (Language -> String) -> [String] -> Aura ()
badReport _ []     = return ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs
