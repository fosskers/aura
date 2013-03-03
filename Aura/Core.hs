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
import Data.List        ((\\), nub, intercalate, isSuffixOf)

import Aura.Settings.Base
import Aura.AurConnection
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Utils
import Aura.Bash

import Bash.Base

import Utilities
import Shell

---

--------
-- TYPES
--------
type ErrMsg = String

-----------
-- PACKAGES
-----------
class Package a where
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

-- I would like to reduce the following three sets of instance declarations
-- to a single more polymorphic solution.
---------------
-- AUR Packages
---------------
data AURPkg = AURPkg String VersionDemand Pkgbuild Namespace
               
instance Package AURPkg where
    pkgNameOf (AURPkg n _ _ _) = n
    versionOf (AURPkg _ v _ _) = v

instance Show AURPkg where
    show = pkgNameWithVersionDemand

instance Eq AURPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

pkgbuildOf :: AURPkg -> String
pkgbuildOf (AURPkg _ _ p _) = p

namespaceOf :: AURPkg -> Namespace
namespaceOf (AURPkg _ _ _ ns) = ns

------------------
-- Pacman Packages
------------------
data PacmanPkg = PacmanPkg String VersionDemand String

instance Package PacmanPkg where
    pkgNameOf (PacmanPkg n _ _) = n
    versionOf (PacmanPkg _ v _) = v

instance Show PacmanPkg where
    show = pkgNameWithVersionDemand

instance Eq PacmanPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

pkgInfoOf :: PacmanPkg -> String
pkgInfoOf (PacmanPkg _ _ i) = i

-------------------
-- Virtual Packages
-------------------
-- Virtual packages also contain a record of their providing package.
-- Providing packages are assumed to be Pacman (ABS) packages.
-- Are there any instances where this isn't the case?
data VirtualPkg = VirtualPkg String VersionDemand (Maybe PacmanPkg)

instance Package VirtualPkg where
    pkgNameOf (VirtualPkg n _ _) = n
    versionOf (VirtualPkg _ v _) = v

instance Show VirtualPkg where
    show = pkgNameWithVersionDemand

providerPkgOf :: VirtualPkg -> Maybe PacmanPkg
providerPkgOf (VirtualPkg _ _ p) = p

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

pacmanPkg :: String -> Aura PacmanPkg
pacmanPkg pkg = PacmanPkg name ver `liftM` pacmanOutput ["-Si",name]
    where (name,ver) = parseNameAndVersionDemand pkg

aurPkg :: String -> Aura AURPkg
aurPkg pkg = do
  pkgbuild  <- downloadPkgbuild name
  namespace <- globals name pkgbuild
  return $ AURPkg name ver pkgbuild namespace
      where (name,ver) = parseNameAndVersionDemand pkg

virtualPkg :: String -> Aura VirtualPkg
virtualPkg pkg = VirtualPkg name ver `liftM` getProvider pkg
    where (name,ver) = parseNameAndVersionDemand pkg
          getProvider n = do
            provider <- getProvidingPkg n
            case provider of
              Nothing -> return Nothing
              Just p  -> Just `liftM` pacmanPkg p

-- Yields a virtual package's providing package if there is one.
getProvidingPkg :: String -> Aura (Maybe String)
getProvidingPkg virt = do
  candidates <- getProvidingPkg' virt
  let lined = lines candidates
  if length lined /= 1
     then return Nothing
     else return . Just . head $ lined

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
-- Adding "$" to the pkg name (technically a regex) fixes a bug.
getProvidingPkg' :: String -> Aura String
getProvidingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  nub `liftM` pacmanOutput ["-Ssq",name ++ "$"]

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

isntMostRecent :: (PkgInfo,String) -> Bool
isntMostRecent (info,v) = trueVer > currVer
  where trueVer = comparableVer $ latestVerOf info
        currVer = comparableVer v

isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> Aura Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

filterAURPkgs :: [String] -> Aura [String]
filterAURPkgs pkgs = map nameOf `liftM` aurInfoLookup pkgs

filterRepoPkgs :: [String] -> Aura [String]
filterRepoPkgs pkgs = do
  repoPkgs <- lines `liftM` pacmanOutput ["-Ssq",pkgs']
  return $ filter (`elem` repoPkgs) pkgs
    where pkgs' = "^(" ++ prep pkgs ++ ")$"
          prep  = specs . intercalate "|"
          specs []     = []
          specs (c:cs) | c `elem` "+" = ['[',c,']'] ++ specs cs
                       | otherwise    = c : specs cs

removePkgs :: [String] -> [String] -> Aura ()
removePkgs [] _         = return ()
removePkgs pkgs pacOpts = pacman  $ ["-Rsu"] ++ pkgs ++ pacOpts

divideByPkgType :: [String] -> Aura ([String],[String],[String])
divideByPkgType pkgs = do
  repoPkgNames <- filterRepoPkgs namesOnly
  aurPkgNames  <- filterAURPkgs $ namesOnly \\ repoPkgNames
  let aurPkgs  = filter (flip elem aurPkgNames . splitName) pkgs
      repoPkgs = filter (flip elem repoPkgNames . splitName) pkgs
      others   = (pkgs \\ aurPkgs) \\ repoPkgs
  return (repoPkgs, aurPkgs, others)
      where namesOnly = map splitName pkgs

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
