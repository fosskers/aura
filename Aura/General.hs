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

module Aura.General where

import Data.List ((\\), nub, intersperse)
import Text.Regex.PCRE ((=~))
import Control.Monad (liftM)

import Aura.Colour.TextColouring
import Aura.Settings.Base
import Aura.AurConnection
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Utils

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
                   | MustBe String
                   | Anything
                     deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = "<"  ++ v
    show (AtLeast v)  = ">=" ++ v
    show (MustBe  v)  = "="  ++ v
    show Anything     = ""

-- I would like to reduce the following three sets of instance declarations
-- to a single more polymorphic solution.
---------------
-- AUR Packages
---------------
data AURPkg = AURPkg String VersionDemand Pkgbuild
               
instance Package AURPkg where
    pkgNameOf (AURPkg n _ _) = n
    versionOf (AURPkg _ v _) = v

instance Show AURPkg where
    show = pkgNameWithVersionDemand

instance Eq AURPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

pkgbuildOf :: AURPkg -> String
pkgbuildOf (AURPkg _ _ p) = p

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
    where (name,comp,ver) = pkg =~ "(<|>=|=)" :: (String,String,String)
          getVersionDemand c v | c == "<"  = LessThan v
                               | c == ">=" = AtLeast v
                               | c == "="  = MustBe v
                               | otherwise = Anything

-- Constructs anything of the Package type.
makePackage :: Package a =>
               (String -> VersionDemand -> t -> a)
               -> (String -> Aura t)
               -> String
               -> Aura a
makePackage typeCon thirdField pkg = typeCon name ver `liftM` thirdField name
    where (name,ver) = parseNameAndVersionDemand pkg

makePacmanPkg :: String -> Aura PacmanPkg
makePacmanPkg pkg = makePackage PacmanPkg getInfo pkg
    where getInfo name = pacmanOutput ["-Si",name]

makeAURPkg :: String -> Aura AURPkg
makeAURPkg pkg = makePackage AURPkg downloadPkgbuild pkg

makeVirtualPkg :: String -> Aura VirtualPkg
makeVirtualPkg pkg = makePackage VirtualPkg getProvider pkg
    where getProvider n = do
            provider <- getProvidingPkg n
            case provider of
              Nothing -> return Nothing
              Just p  -> Just `liftM` makePacmanPkg p

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
-- Action won't be allowed unless user is root, or using sudo.
sudo :: Aura () -> Aura ()
sudo action = do
  hasPerms <- (hasRootPriv . environmentOf) `liftM` ask
  if hasPerms
     then action
     else scoldAndFail mustBeRootMsg1

-- Prompt if the user is the true Root. Building as it can be dangerous.
trueRoot :: Aura () -> Aura ()
trueRoot action = ask >>= \ss -> do
  if isntTrueRoot $ environmentOf ss
     then action
     else do
       okay <- optionalPrompt trueRootMsg1
       if okay
          then action
          else notify trueRootMsg2

getForeignPackages :: Aura [(String,String)]
getForeignPackages = map fixName `liftM` lines `liftM` pacmanOutput ["-Qm"]
    where fixName = hardBreak (== ' ')

isntMostRecent :: (PkgInfo,String) -> Bool
isntMostRecent (info,v) = trueVer > currVer
  where trueVer = comparableVer $ latestVerOf info
        currVer = comparableVer v

isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> Aura Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

-- Beautiful.
filterAURPkgs :: [String] -> Aura [String]
filterAURPkgs pkgs = map nameOf `liftM` aurInfoLookup pkgs

filterRepoPkgs :: [String] -> Aura [String]
filterRepoPkgs pkgs = do
  repoPkgs <- pacmanOutput ["-Ssq",pkgs']
  return . filter (`elem` pkgs) . lines $ repoPkgs
    where pkgs' = "^(" ++ prep pkgs ++ ")$"
          prep  = specs . concat . intersperse "|"
          specs []     = []
          specs (c:cs) | c `elem` "+" = ['[',c,']'] ++ specs cs
                       | otherwise    = c : specs cs

getOrphans :: Aura [String]
getOrphans = lines `liftM` pacmanOutput ["-Qqdt"]

removePkgs :: [String] -> [String] -> Aura ()
removePkgs [] _         = return ()
removePkgs pkgs pacOpts = pacman  $ ["-Rsu"] ++ pkgs ++ pacOpts

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------
colouredMessage :: Colouror -> (Language -> String) -> Aura ()
colouredMessage c msg = ask >>= putStrLnA c . msg . langOf

renderColour :: Colouror -> (Language -> String) -> Aura String
renderColour c msg = ask >>= return . c . msg . langOf

say :: (Language -> String) -> Aura ()
say msg = colouredMessage noColour msg

notify :: (Language -> String) -> Aura ()
notify msg = colouredMessage green msg

warn :: (Language -> String) -> Aura ()
warn msg = colouredMessage yellow msg

scold :: (Language -> String) -> Aura ()
scold msg = colouredMessage red msg

scoldAndFail :: (Language -> String) -> Aura a
scoldAndFail msg = do
  lang <- langOf `liftM` ask
  failure . putStrA' red . msg $ lang

badReport :: (Language -> String) -> [String] -> Aura ()
badReport _ []     = return ()
badReport msg pkgs = ask >>= \ss -> printList red cyan (msg $ langOf ss) pkgs

divideByPkgType :: [String] -> Aura ([String],[String],[String])
divideByPkgType pkgs = do
  repoPkgNames <- filterRepoPkgs namesOnly
  aurPkgNames  <- filterAURPkgs $ namesOnly \\ repoPkgNames
  let aurPkgs  = filter (flip elem aurPkgNames . splitName) pkgs
      repoPkgs = filter (flip elem repoPkgNames . splitName) pkgs
      others   = (pkgs \\ aurPkgs) \\ repoPkgs
  return (repoPkgs, aurPkgs, others)
      where namesOnly = map splitName pkgs

-- Format two lists into two nice rows a la `-Qi` or `-Si`.
entrify :: Settings -> [String] -> [String] -> String
entrify ss fs es = concat $ intersperse "\n" fsEs
    where fsEs = map combine $ zip fs' es
          fs'  = padding ss fs
          combine (f,e) = f ++ " : " ++ e

-- Right-pads strings according to the longest string in the group.
padding :: Settings -> [String] -> [String]
padding ss fs = map (\x -> postPad x ws longest) fs
    where ws      = whitespace $ langOf ss
          longest = maximum $ map length fs
