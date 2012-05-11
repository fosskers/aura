-- A library for dealing with the downloading and installion
-- of Arch Linux packages.
-- Written after studying the `packer` source code.

module ArchPackages where

import System.Directory (getHomeDirectory, doesFileExist)
import Control.Monad (filterM)
import Utilities
import Pacman

type Package = String

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

makePkgConfFile :: FilePath
makePkgConfFile = "/etc/makepkg.conf"

userMakePkgConfFile :: FilePath -> FilePath
userMakePkgConfFile home = home ++ "/.makepkg.conf"

chooseMakePkgConfFile :: IO FilePath
chooseMakePkgConfFile = do
  home         <- getHomeDirectory
  let userConfFile = userMakePkgConfFile home
  exists       <- doesFileExist userConfFile
  if exists
  then return userConfFile
  else return makePkgConfFile

depsToInstall :: Package -> IO [Package]
depsToInstall pkg = do
  deps <- determineDeps pkg
  filterM isNotInstalled deps

-- Recursion necessary here. 
-- What about circular deps?
determineDeps :: Package -> IO [Package]
determineDeps pkg = undefined

isInstalled :: Package -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

isNotInstalled :: Package -> IO Bool
isNotInstalled pkg = do
  installed <- isInstalled pkg
  return $ not installed

isArchPackage :: Package -> IO Bool
isArchPackage pkg = pacmanSuccess ["-Si",pkg]

isAURPackage :: Package -> IO Bool
isAURPackage = undefined

getInstalledPackageDesc :: Package -> IO [(String,String)]
getInstalledPackageDesc pkg = do
  installed <- isInstalled pkg
  if installed
  then pacman ["-Qi",pkg] >>= return . getPackageDesc . tripleSnd
  else error $ pkg ++ " is not installed!"

getNewestPackageDesc :: Package -> IO [(String,String)]
getNewestPackageDesc pkg =
    pacman ["-Si",pkg] >>= return . getPackageDesc . tripleSnd
    
-- This isn't correct! Dependencies line wrap!
-- Parses a package description from some source into an association list.
getPackageDesc :: String -> [(String,String)]
getPackageDesc = map (cleanFields . hardBreak (== ':')) . lines
    where cleanFields (x,y) = (rStrip x, lStrip y)