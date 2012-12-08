{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

import Data.List ((\\), nub, sortBy, intersperse)
import System.Exit (ExitCode(..))
import Text.Regex.PCRE ((=~))
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Char (isDigit)

import Aura.AurConnection
import Aura.Languages
import Aura.Settings
import Aura.Pacman
import Utilities
import Shell
import Zero

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
               -> (String -> IO t)
               -> String
               -> IO a
makePackage typeCon thirdField pkg = typeCon name ver `liftM` thirdField name
    where (name,ver) = parseNameAndVersionDemand pkg

makePacmanPkg :: String -> IO PacmanPkg
makePacmanPkg pkg = makePackage PacmanPkg getInfo pkg
    where getInfo name = pacmanOutput ["-Si",name]

makeAURPkg :: String -> IO AURPkg
makeAURPkg pkg = makePackage AURPkg downloadPkgbuild pkg

makeVirtualPkg :: String -> IO VirtualPkg
makeVirtualPkg pkg = makePackage VirtualPkg getProvider pkg
    where getProvider n =
            getProvidingPkg n ?>>= (Just `liftM`) . makePacmanPkg . fromJust

-- Yields a virtual package's providing package if there is one.
getProvidingPkg :: String -> IO (Maybe String)
getProvidingPkg virt = do
  candidates <- getProvidingPkg' virt
  let lined = lines candidates
  return (length lined == 1) ?>> (return . Just . head $ lined)

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
-- Adding "$" to the pkg name (technically a regex) fixes a bug.
getProvidingPkg' :: String -> IO String
getProvidingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  nub `liftM` pacmanOutput ["-Ssq",name ++ "$"]

------------
-- OPERATORS
------------
-- IO action won't be allowed unless user is root, or using [$]udo.
(|$|) :: Settings -> IO ExitCode -> IO ExitCode
settings |$| action = mustBeRoot settings action

mustBeRoot :: Settings -> IO ExitCode -> IO ExitCode
mustBeRoot settings action
  | hasRootPriv $ environmentOf settings = action
  | otherwise = scoldAndFail settings mustBeRootMsg1

-- Prompt if the user is the [+]rue Root. This can be dangerous.
(|+|) :: Settings -> IO ExitCode -> IO ExitCode
settings |+| action = trueRootCheck settings action

trueRootCheck :: Settings -> IO ExitCode -> IO ExitCode
trueRootCheck ss action
    | isntTrueRoot $ environmentOf ss = action
    | otherwise = do
    okay <- optionalPrompt (mustConfirm ss) (trueRootCheckMsg1 $ langOf ss)
    if okay
       then action
       else notify ss trueRootCheckMsg2 >> returnSuccess

getForeignPackages :: IO [(String,String)]
getForeignPackages = map fixName `liftM` lines `liftM` pacmanOutput ["-Qm"]
    where fixName = hardBreak (== ' ')

isntMostRecent :: (PkgInfo,String) -> Bool
isntMostRecent (info,v) = trueVer > currVer
  where trueVer = comparableVer $ latestVerOf info
        currVer = comparableVer v

isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

-- Beautiful.
filterAURPkgs :: [String] -> IO [String]
filterAURPkgs pkgs = aurInfoLookup pkgs ?>>= return . map nameOf . fromRight

filterRepoPkgs :: [String] -> IO [String]
filterRepoPkgs pkgs = do
  repoPkgs <- pacmanOutput ["-Ssq",pkgs']
  return . filter (`elem` pkgs) . lines $ repoPkgs
    where pkgs' = "^(" ++ prep pkgs ++ ")$"
          prep  = specs . concat . intersperse "|"
          specs []     = []
          specs (c:cs) | c `elem` "+" = ['[',c,']'] ++ specs cs
                       | otherwise    = c : specs cs

getOrphans :: IO [String]
getOrphans = lines `liftM` pacmanOutput ["-Qqdt"]

removePkgs :: Settings -> [String] -> [String] -> IO ExitCode
removePkgs _ [] _          = returnSuccess
removePkgs ss pkgs pacOpts = pacman ss $ ["-Rsu"] ++ pkgs ++ pacOpts

-------
-- MISC  -- Too specific for `Utilities.hs`
-------
colouredMessage :: Colouror -> Settings -> (Language -> String) -> IO ()
colouredMessage c settings msg = putStrLnA c . msg . langOf $ settings

say :: Settings -> (Language -> String) -> IO ()
say settings msg = colouredMessage noColour settings msg

notify :: Settings -> (Language -> String) -> IO ()
notify settings msg = colouredMessage green settings msg

warn :: Settings -> (Language -> String) -> IO ()
warn settings msg = colouredMessage yellow settings msg

scold :: Settings -> (Language -> String) -> IO ()
scold settings msg = colouredMessage red settings msg

scoldAndFail :: Settings -> (Language -> String) -> IO ExitCode
scoldAndFail settings msg = scold settings msg >> return (ExitFailure 1)

splitNameAndVer :: String -> (String,String)
splitNameAndVer pkg = (before,after)
    where (before,_,after) = (pkg =~ "[<>=]+" :: (String,String,String))

splitName :: String -> String
splitName = fst . splitNameAndVer

splitVer :: String -> String
splitVer = snd . splitNameAndVer

-- Used for folding.
groupPkgs :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
groupPkgs (ps,as,os) (p,a,o) = (p ++ ps, a ++ as, o ++ os)

divideByPkgType :: [String] -> IO ([String],[String],[String])
divideByPkgType pkgs = do
  repoPkgNames <- filterRepoPkgs namesOnly
  aurPkgNames  <- filterAURPkgs $ namesOnly \\ repoPkgNames
  let aurPkgs  = filter (flip elem aurPkgNames . splitName) pkgs
      repoPkgs = filter (flip elem repoPkgNames . splitName) pkgs
      others   = (pkgs \\ aurPkgs) \\ repoPkgs
  return (repoPkgs, aurPkgs, others)
      where namesOnly = map splitName pkgs

sortPkgs :: [String] -> [String]
sortPkgs pkgs = sortBy verNums pkgs
    where verNums a b | name a /= name b = compare a b  -- Different pkgs
                      | otherwise        = compare (ver a) (ver b)
          name = fst . pkgFileNameAndVer
          ver  = snd . pkgFileNameAndVer

-- linux-3.2.14-1-x86_64.pkg.tar.xz    -> ("linux",[3,2,14,1])
-- wine-1.4rc6-1-x86_64.pkg.tar.xz     -> ("wine",[1,4,6,1])
-- ruby-1.9.3_p125-4-x86_64.pkg.tar.xz -> ("ruby",[1,9,3,125,4])
-- NOTE: regex stuff is a little sloppy here.
pkgFileNameAndVer :: String -> (String,[Int])
pkgFileNameAndVer p = (name,verNum')
    where (name,_,_) = p =~ "-[0-9]+" :: (String,String,String)
          verNum     = p =~ ("[0-9][-0-9a-z._]+-" ++ archs) :: String
          archs      = "(a|x|i)"  -- Representing "(any|x86_64|i686)"
          verNum'    = comparableVer verNum

-- Also discards any non-number version info, like `rc`, etc.
-- Example: "3.2rc6-1" becomes [3,2,6,1]
comparableVer :: String -> [Int]
comparableVer [] = []
comparableVer n  =
    case dropWhile (not . isDigit) n of
      []   -> []  -- Version ended in non-digits.
      rest -> read digits : (comparableVer $ drop (length digits) rest)
        where digits = takeWhile isDigit rest

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
