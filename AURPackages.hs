-- A library for dealing with the installion and management
-- of Arch User Repository packages.

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

type ErrorMsg = String

data Versioning = AtLeast String | MustBe String | Anything deriving (Eq)

data Package = Package { pkgNameOf  :: String
                       , versionOf  :: Versioning
                       , pkgbuildOf :: Pkgbuild 
                       } 
               
instance Show Package where
    show = pkgNameWithVersion

instance Eq Package where
    a == b = pkgNameWithVersion a == pkgNameWithVersion b

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
            
-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> IO FilePath
moveToCache pkg = renameFile pkg newName >> return newName
    where newName = packageCache </> pkg

-- Returns either a list of error message or the deps to be installed.
{-
Get each package's deps and fold the together.
filterM a `mustInstall` across all the deps. 
Check for the various kinds of conflicts:
  Pacman: Package is ignored.
          Version demanded is not installed, nor is it the same as the
          latest version.
  AUR: Package is ignored.
       Version demanded is different from the one given in its PKGBUILD.
  Virtual: Parent package is ignored.
           Version demanded is not provided by the lastest version of
           its parent package.
Conflict checking returns `Maybe` values. If a `Just` is found _anywhere_
then the entire dep check has to fail. Return a `Left` value with
the appropriate conflict messages.
If everything is okay, then return all the packages.
-}
getDepsToInstall :: Language -> [Package] -> [String] -> IO (Either [ErrorMsg] ([String],[Package]))
getDepsToInstall lang pkgs toIgnore = do
  allDeps <- mapM (determineDeps lang) pkgs  -- Can this be done with foldM?
  let (ps,as,vs) = foldl groupPkgs ([],[],[]) allDeps
  necPacPkgs <- filterM mustInstall ps
  necAURPkgs <- filterM (mustInstall . show) as
  necVirPkgs <- filterM mustInstall vs
  conflicts  <- getConflicts necPacPkgs necAURPkgs necVirPkgs
  if not $ null conflicts
     then return $ Left conflicts
     else do
       providers <- mapM getProvidingPkg' necVirPkgs
       return $ Right (nub $ providers ++ necPacPkgs, necAURPkgs)

-- Returns ([PacmanPackages], [AURPackages], [VirtualPackages])
determineDeps :: Language -> Package -> IO ([String],[Package],[String])
determineDeps lang pkg = do
  let depNames = (getPkgbuildField "depends" $ pkgbuildOf pkg) ++
                 (getPkgbuildField "makedepends" $ pkgbuildOf pkg)
  (archPkgNames,aurPkgNames,other) <- divideByPkgType depNames
  aurPkgs       <- mapM packagify aurPkgNames
  recursiveDeps <- mapM (determineDeps lang) aurPkgs
  let (ps,as,os) = foldl groupPkgs (archPkgNames,aurPkgs,other) recursiveDeps
  return (nub ps, nub as, nub os)

getConflicts :: [String] -> [Package] -> [String] -> IO [ErrorMsg]
getConflicts ps as vs = undefined

-- Used for folding.
groupPkgs :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
groupPkgs (ps,as,os) (p,a,o) = (p ++ ps, a ++ as, o ++ os)

-- This could be slow, depending on internet speeds, etc.
divideByPkgType :: [String] -> IO ([String],[String],[String])
divideByPkgType pkgs = do
  archPkgs <- filterM (isArchPackage . stripVerNum) pkgs
  let remaining = pkgs \\ archPkgs
  aurPkgs  <- filterM (isAURPackage . stripVerNum) remaining
  return (archPkgs, aurPkgs, remaining \\ aurPkgs)
      where stripVerNum = fst . splitNameAndVer

-- Note: Make sure to run this on the Virtual Packages first.
mustInstall :: String -> IO Bool
mustInstall pkg = do
  necessaryDeps <- pacmanOutput $ ["-T",pkg]
  return $ length (words necessaryDeps) == 1

-- Checks for pkg=specificvernum. Freaks out if this is different from
-- the version number given from `pacman -Si`.
-- Also checks for Ignored Packages.
getPacmanConflicts :: Language -> [String] -> String -> String -> Maybe String
getPacmanConflicts lang toIgnore info pkg
    | pkg `elem` toIgnore = Just failMessage1
    | '=' `elem` pkg && recVer /= reqVer = Just failMessage2
    | otherwise = Nothing    
    where recVer        = getMostRecentVerNum info
          (name,reqVer) = splitNameAndVer pkg
          failMessage1  = getPacmanConflictsMsg2 lang pkg
          failMessage2  = getPacmanConflictsMsg1 lang name recVer reqVer
       
-- Takes `pacman -Si` output as input
getMostRecentVerNum :: String -> String
getMostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info

splitNameAndVer :: String -> (String,String)
splitNameAndVer pkg = (before,after)
    where (before,_,after) = (pkg =~ "[<>=]+" :: (String,String,String))

-- Yields a virtual package's providing package if there is one.
getProvidingPkg :: String -> IO (Maybe String)
getProvidingPkg virt = do
  candidates <- getProvidingPkg' virt
  let lined = lines candidates
  if length lined /= 1
     then return Nothing
     else return . Just . head $ lined

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
getProvidingPkg' :: String -> IO String
getProvidingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  pacmanOutput ["-Ssq",name]
  
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
reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages _ []      = return ()
reportNonPackages lang nons = do
  putStrLnA $ reportNonPackagesMsg1 lang
  mapM_ putStrLn nons

-- Same as the function above... 
reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages _ []      = return ()
reportIgnoredPackages lang pkgs = do
  putStrLnA $ reportIgnoredPackagesMsg1 lang
  mapM_ putStrLn pkgs

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
