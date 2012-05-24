-- A library for dealing with the downloading and installion
-- of Arch User Repository packages.
-- Written after studying the `packer` source code.

module AURPackages where

-- System Libraries
import System.Directory (getHomeDirectory, doesFileExist, renameFile)
import System.FilePath ((</>))
import Control.Monad (filterM)
import System.Exit (ExitCode(..))

-- Custom Libraries
import AURLanguages
import Utilities
import MakePkg
import Pacman

type Package = String

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
installPackageFiles :: [FilePath] -> IO ExitCode
-- installPackageFiles []    = return $ ExitFailure 1
installPackageFiles files = pacman $ ["-U"] ++ files

-- Handles the building of Packages.
-- Assumed: All pacman dependencies are already installed.
--          All AUR dependencies have been added so that they come first.
buildPackages :: Language -> [Package] -> IO [FilePath]
buildPackages _ []        = return []
buildPackages lang (p:ps) = do
  putStrLnA $ buildPackagesMsg1 lang p
  results <- withTempDir p (build p)
  case results of
    (Just pkg,_) -> buildPackages lang ps >>= return . (\pkgs -> pkg : pkgs)
    (Nothing,output) -> do
        putStrLnA $ buildPackagesMsg2 lang p
        putStrA $ buildPackagesMsg3 lang
        timedMessage 1000000 ["3.. ","2.. ","1..\n"]
        putStrLn output
        putStrLnA $ buildPackagesMsg4 lang
        mapM_ putStrLn ps
        putStrLnA $ buildPackagesMsg5 lang
        answer <- yesNoPrompt (buildPackagesMsg6 lang) "^y"
        if answer then return [] else error (buildPackagesMsg7 lang)
        
build :: Package -> IO (Maybe FilePath, String)
build pkg = do
  pkgbuildPath <- downloadPKGBUILD pkg
  (exitStatus,pkgName,output) <- makepkg pkgbuildPath
  if didProcessSucceed exitStatus
  then moveToCache pkgName >>= return . (\pkg -> (Just pkg,output))
  else return (Nothing,output)
            
downloadPKGBUILD :: Package -> IO FilePath
downloadPKGBUILD = undefined

-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> IO FilePath
moveToCache pkg = renameFile pkg newName >> return newName
    where newName = packageCache </> pkg

depsToInstall :: Package -> IO [Package]
depsToInstall pkg = do
  deps <- determineDeps pkg
  filterM isNotInstalled deps

-- Check all build-deps and runtime-deps first. Install those first.
-- Deps were: Arch packages -> Collect them all then run a batch `pacman`.
--            AUR  packages -> Add them to the [Package] list. At the head?
--                             Or else the later builds won't work?
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

getOrphans :: IO [String]
getOrphans = pacmanQuiet ["-Qqdt"] >>= return . lines . tripleSnd

-- These might not be necessary.
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
