-- A library for dealing with the downloading and installion
-- of Arch Linux packages.
-- Written after studying the `packer` source code.

module ArchPackages where

import System.Directory (getHomeDirectory, doesFileExist)
import Utilities (filterM, didProcessSucceed)

type Package = String

pacman :: String
pacman = "pacman"

pacmanConf :: FilePath
pacmanConf = "/etc/pacman.conf"

makePkgConf :: FilePath
makePkgConf = "/etc/makepkg.conf"

userMakePkgConf :: FilePath -> FilePath
userMakePkgConf home = home ++ "/.makepkg.conf"

chooseMakePkgConf :: IO FilePath
chooseMakePkgConf = do
  home     <- getHomeDirectory
  userConf <- userMakePkgConf home
  exists   <- doesFileExist userConf
  if exists
  then return userConf
  else return makePkgConf

depsToInstall :: Package -> IO [Package]
depsToInstall pkg = do
  deps <- determineDeps pkg
  filterM isNotInstalled deps

determineDeps :: Package -> IO [Package]
determineDeps pkg = undefined

isInstalled :: Package -> IO Bool
isInstalled pkg = didProcessSucceed pacman ["-Qq",pkg] []

isNotInstalled :: Package -> IO Bool
isNotInstalled pkg = do
  installed <- isInstalled pkg
  return $ not installed

isArchPackage :: Package -> IO Bool
isArchPackage pkg = didProcessSucceed pacman ["-Si",pkg] [] 

isAURPackage :: Package -> IO Bool
isAURPackage = undefined

