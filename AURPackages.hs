-- A library for dealing with the downloading and installion
-- of Arch User Repository packages.
-- Written after studying the `packer` source code.

module AURPackages where

-- System Libraries
import System.Directory (renameFile)
import System.FilePath ((</>))
import Control.Monad (filterM)
import Text.Regex.Posix ((=~))
import Data.List ((\\), nub)

-- Custom Libraries
import AuraLanguages
import Utilities
import Pkgbuild
import Internet
import MakePkg
import Pacman

data Versioning = AtLeast String | MustBe String | Anything deriving (Eq)

data Package = Package { pkgNameOf  :: String
                       , versionOf  :: Versioning
                       , pkgbuildOf :: Pkgbuild 
                       } deriving (Eq)
               
instance Show Package where
    show = pkgNameOf

-- This will explode if the package doesn't exist.
packagify :: String -> IO Package
packagify pkg = do
  let (name,comp,ver) = pkg =~ "(>=|=)" :: (String,String,String)
      versioning      = getVersioning comp ver
  pkgbuild <- downloadPkgbuild name
  return $ Package name versioning pkgbuild
      where getVersioning c v | c == ">=" = AtLeast v
                              | c == "="  = MustBe v
                              | otherwise = Anything

pkgNameWithVersion :: Package -> String
pkgNameWithVersion pkg = pkgNameOf pkg ++ signAndVersion
    where signAndVersion = case versionOf pkg of
                             AtLeast v -> ">=" ++ v
                             MustBe  v -> "="  ++ v
                             Anything  -> ""

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
  case didProcessSucceed exitStatus of
    True -> moveToCache pkgName >>= return . (\pkg -> (Just pkg,output))
    _    -> return (Nothing,output)
  {-
  if didProcessSucceed exitStatus
  then moveToCache pkgName >>= return . (\pkg -> (Just pkg,output))
  else return (Nothing,output)
-}
            
-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> IO FilePath
moveToCache pkg = renameFile pkg newName >> return newName
    where newName = packageCache </> pkg

getDepsToInstall :: [Package] -> IO ([String],[Package])
getDepsToInstall pkgs = undefined

determineDeps :: Package -> IO ([String],[Package])
determineDeps pkg = do
  let depNames   = (getPkgbuildField "depends" $ pkgbuildOf pkg) ++
                   (getPkgbuildField "makedepends" $ pkgbuildOf pkg)
  aurPkgNames   <- filterM (isAURPackage . stripVerNum) depNames
  aurPkgs       <- mapM packagify aurPkgNames
  recursiveDeps <- mapM determineDeps aurPkgs
  let allDeps = foldl fuse (depNames \\ aurPkgNames,aurPkgs) recursiveDeps
  return $ (nub $ fst allDeps, nub $ snd allDeps)
      where stripVerNum        = takeWhile (`notElem` "<>=")
            fuse (ps,as) (p,a) = (p ++ ps, a ++ as)

isInstalled :: String -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

isNotInstalled :: String -> IO Bool
isNotInstalled pkg = pacmanFailure ["-Qq",pkg]

isArchPackage :: String -> IO Bool
isArchPackage pkg = pacmanSuccess ["-Si",pkg]

-- A package is an AUR package if it's PKGBUILD exists on the Arch website.
isAURPackage :: String -> IO Bool
isAURPackage = doesUrlExist . getPkgbuildUrl

-- TODO: Add guesses! "Did you mean xyz instead?"
handleNonPackages :: Language -> [String] -> IO ()
handleNonPackages lang nons = do
  putStrLnA $ handleNonPackagesMsg1 lang
  mapM_ putStrLn nons

getInstalledAurPackages :: IO [String]
getInstalledAurPackages = pacmanOutput ["-Qm"] >>= return . lines

-- These might not be necessary.
{-
getOrphans :: IO [String]
getOrphans = pacmanOutput ["-Qqdt"] >>= return . lines

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
