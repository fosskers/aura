-- A library for dealing with the downloading and installion
-- of Arch User Repository packages.
-- Written after studying the `packer` source code.

module AURPackages where

-- System Libraries
import System.Directory (renameFile)
import System.FilePath ((</>))
import Control.Monad (filterM)
import System.Exit (ExitCode(..))

-- Custom Libraries
import AuraLanguages
import Utilities
import Internet
import MakePkg
import Pacman

data Package = Package { pkgNameOf :: String, pkgbuildOf :: String}
               deriving (Eq)

instance Show Package where
    show = pkgNameOf

packagify :: String -> IO Package
packagify pkg = do
  pkgbuild <- downloadPkgbuild pkg
  return $ Package pkg pkgbuild

packageCache :: FilePath
packageCache = "/var/cache/pacman/pkg/"

{- Not certain if these are necessary yet.
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
-}

-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.gz
installPackageFiles :: [FilePath] -> IO ()
-- installPackageFiles []    = return $ ExitFailure 1
installPackageFiles files = pacman $ ["-U"] ++ files

-- Handles the building of Packages.
-- Assumed: All pacman dependencies are already installed.
--          All AUR dependencies have been added so that they come first.
buildPackages :: Language -> [Package] -> IO [FilePath]
buildPackages _ []        = return []
buildPackages lang (p:ps) = do
  putStrLnA $ buildPackagesMsg1 lang (show p)
  results <- withTempDir (show p) (build p)
  case results of
    (Just pkg,_) -> buildPackages lang ps >>= return . (\pkgs -> pkg : pkgs)
    (Nothing,output) -> do
        putStrLnA $ buildPackagesMsg2 lang (show p)
        putStrA $ buildPackagesMsg3 lang
        timedMessage 1000000 ["3.. ","2.. ","1..\n"]
        putStrLn output
        putStrLnA $ buildPackagesMsg4 lang
        mapM_ (putStrLn . show) ps
        putStrLnA $ buildPackagesMsg5 lang
        answer <- yesNoPrompt (buildPackagesMsg6 lang) "^y"
        if answer then return [] else error (buildPackagesMsg7 lang)
        
build :: Package -> IO (Maybe FilePath, String)
build pkg = do
  writeFile "PKGBUILD" $ pkgbuildOf pkg
  (exitStatus,pkgName,output) <- makepkg []
  if didProcessSucceed exitStatus
  then moveToCache pkgName >>= return . (\pkg -> (Just pkg,output))
  else return (Nothing,output)
            
-- Assumption: The package given EXISTS as an AUR package.
--             Non-existant packages should have been filtered out by now.
downloadPkgbuild :: String -> IO String
downloadPkgbuild = getUrlContents . getPkgbuildUrl

-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> IO FilePath
moveToCache pkg = renameFile pkg newName >> return newName
    where newName = packageCache </> pkg

{-
depsToInstall :: Package -> IO [Package]
depsToInstall pkg = do
  deps <- determineDeps pkg
  filterM (isNotInstalled . show) deps
-}

-- Check all build-deps and runtime-deps first. Install those first.
-- Deps were: Arch packages -> Collect them all then run a batch `pacman`.
--            AUR  packages -> Add them to the [Package] list. At the head?
--                             Or else the later builds won't work?
-- What about circular deps?
determineDeps :: [Package] -> IO ([String],[Package])
determineDeps pkg = undefined

isInstalled :: String -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

isNotInstalled :: String -> IO Bool
isNotInstalled pkg = do
  installed <- isInstalled pkg
  return $ not installed

isArchPackage :: String -> IO Bool
isArchPackage pkg = pacmanSuccess ["-Si",pkg]

isAURPackage :: String -> IO Bool
isAURPackage = doesUrlExist . getPkgbuildUrl

getPkgbuildUrl :: String -> Url
getPkgbuildUrl pkg = "https://aur.archlinux.org/packages/" </>
                     take 2 pkg </> pkg </> "PKGBUILD"

-- TODO: Add guesses! "Did you mean xyz instead?"
handleNonPackages :: Language -> [String] -> IO ()
handleNonPackages lang nons = do
  putStrLnA $ handleNonPackagesMsg1 lang
  mapM_ putStrLn nons

-- These might not be necessary.
{-
getOrphans :: IO [String]
getOrphans = pacmanQuiet ["-Qqdt"] >>= return . lines . tripleSnd

getInstalledPackageDesc :: Package -> IO [(String,String)]
getInstalledPackageDesc pkg = do
  installed <- isInstalled pkg
  if installed
  then pacmanQuiet ["-Qi",pkg] >>= return . getPackageDesc . tripleSnd
  else error $ pkg ++ " is not installed!"

getNewestPackageDesc :: Package -> IO [(String,String)]
getNewestPackageDesc pkg =
    pacmanQuiet ["-Si",pkg] >>= return . getPackageDesc . tripleSnd
    
-- This isn't correct! Dependencies line wrap!
-- Parses a package description from some source into an association list.
getPackageDesc :: String -> [(String,String)]
getPackageDesc = map (cleanFields . hardBreak (== ':')) . lines
    where cleanFields (x,y) = (rStrip x, lStrip y)
-}
