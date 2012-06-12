-- A library for dealing with the installion and management
-- of Arch User Repository packages.

module AuraLib where

-- System Libraries
import System.Directory (renameFile)
import System.FilePath ((</>))
import Control.Monad (filterM)
import Text.Regex.Posix ((=~))
import Data.List ((\\), nub)
import Data.Maybe (fromJust)

-- Custom Libraries
import AuraLanguages
import Utilities
import Pkgbuild
import Internet
import MakePkg
import Pacman

type ErrorMsg = String

class Package a where
    pkgNameOf :: a -> String
    versionOf :: a -> Versioning                 

data Versioning = AtLeast String | MustBe String | Anything deriving (Eq)

---------------
-- AUR Packages
---------------
data AURPkg = AURPkg String Versioning Pkgbuild
               
instance Package AURPkg where
    pkgNameOf (AURPkg n _ _) = n
    versionOf (AURPkg _ v _) = v

instance Show AURPkg where
    show = pkgNameWithVersion

instance Eq AURPkg where
    a == b = pkgNameWithVersion a == pkgNameWithVersion b

pkgbuildOf :: AURPkg -> String
pkgbuildOf (AURPkg _ _ p) = p

------------------
-- Pacman Packages
------------------
data PacmanPkg = PacmanPkg String Versioning String

instance Package PacmanPkg where
    pkgNameOf (PacmanPkg n _ _) = n
    versionOf (PacmanPkg _ v _) = v

instance Show PacmanPkg where
    show = pkgNameWithVersion

pkgInfoOf :: PacmanPkg -> String
pkgInfoOf (PacmanPkg _ _ i) = i

-------------------
-- Virtual Packages
-------------------
data VirtualPkg = VirtualPkg String Versioning (Maybe String)

instance Package VirtualPkg where
    pkgNameOf (VirtualPkg n _ _) = n
    versionOf (VirtualPkg _ v _) = v

instance Show VirtualPkg where
    show = pkgNameWithVersion

providerPkgOf :: VirtualPkg -> Maybe String
providerPkgOf (VirtualPkg _ _ p) = p

---------------------------------
-- Functions common to `Package`s
---------------------------------
pkgNameWithVersion :: Package a => a -> String
pkgNameWithVersion pkg = pkgNameOf pkg ++ signAndVersion
    where signAndVersion = case versionOf pkg of
                             AtLeast v -> ">=" ++ v
                             MustBe  v -> "="  ++ v
                             Anything  -> ""

parseNameAndVersioning :: String -> (String,Versioning)
parseNameAndVersioning pkg = (name, getVersioning comp ver)
    where (name,comp,ver) = pkg =~ "(>=|=)" :: (String,String,String)
          versioning      = getVersioning comp ver
          getVersioning c v | c == ">=" = AtLeast v
                            | c == "="  = MustBe v
                            | otherwise = Anything

makePacmanPkg :: String -> IO PacmanPkg
makePacmanPkg pkg = do
  let (name,ver) = parseNameAndVersioning pkg
  info <- pacmanOutput ["-Si",name]
  return $ PacmanPkg name ver info

makeAURPkg :: String -> IO AURPkg
makeAURPkg pkg = do
  let (name,ver) = parseNameAndVersioning pkg
  pkgbuild <- downloadPkgbuild pkg
  return $ AURPkg name ver pkgbuild

makeVirtualPkg :: String -> IO VirtualPkg
makeVirtualPkg pkg = do
  let (name,ver) = parseNameAndVersioning pkg
  provider <- getProvidingPkg name
  return $ VirtualPkg name ver provider

-----------
-- The Work
-----------
-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.gz
installPackageFiles :: [FilePath] -> IO ()
installPackageFiles files = pacman $ ["-U"] ++ files

-- Handles the building of Packages.
-- Assumed: All pacman dependencies are already installed.
--          All AUR dependencies have been added so that they come first.
buildPackages :: Language -> [AURPkg] -> IO [FilePath]
buildPackages _ []        = return []
buildPackages lang (p:ps) = do
  putStrLnA $ buildPackagesMsg1 lang (show p)
  results <- withTempDir (show p) (build p)
  case results of
    Right pkg   -> buildPackages lang ps >>= return . (\pkgs -> pkg : pkgs)
    Left output -> do
        putStrLnA $ buildPackagesMsg2 lang (show p)
        putStrA $ buildPackagesMsg3 lang
        timedMessage 1000000 ["3.. ","2.. ","1..\n"]
        putStrLn output
        putStrLnA $ buildPackagesMsg4 lang
        mapM_ (putStrLn . show) ps
        putStrLnA $ buildPackagesMsg5 lang
        answer <- yesNoPrompt (buildPackagesMsg6 lang) "^y"
        if answer then return [] else error (buildPackagesMsg7 lang)
        
build :: AURPkg -> IO (Either String FilePath)
build pkg = do
  writeFile "PKGBUILD" $ pkgbuildOf pkg
  (exitStatus,pkgName,output) <- makepkg []
  if didProcessSucceed exitStatus
     then moveToCache pkgName >>= return . (\pkg -> Right pkg)
     else return $ Left output
            
-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> IO FilePath
moveToCache pkg = renameFile pkg newName >> return newName
    where newName = packageCache </> pkg

-- Returns either a list of error message or the deps to be installed.
getDepsToInstall :: Language -> [AURPkg] -> [String] ->
                    IO (Either [ErrorMsg] ([String],[AURPkg]))
getDepsToInstall lang pkgs toIgnore = do
  allDeps <- mapM (determineDeps lang) pkgs  -- Can this be done with foldM?
  let (ps,as,vs) = foldl groupPkgs ([],[],[]) allDeps
  necPacPkgs <- filterM mustInstall ps >>= mapM makePacmanPkg
  necAURPkgs <- filterM (mustInstall . show) as
  necVirPkgs <- filterM mustInstall vs >>= mapM makeVirtualPkg
  let conflicts = getConflicts lang toIgnore necPacPkgs necAURPkgs necVirPkgs
  if not $ null conflicts
     then return $ Left conflicts
     else do
       let providers  = map (fromJust . providerPkgOf) necVirPkgs
           pacmanPkgs = map pkgNameOf necPacPkgs
       return $ Right (nub $ providers ++ pacmanPkgs, necAURPkgs)

-- Returns ([PacmanPackages], [AURPackages], [VirtualPackages])
determineDeps :: Language -> AURPkg -> IO ([String],[AURPkg],[String])
determineDeps lang pkg = do
  let depNames = (getPkgbuildField "depends" $ pkgbuildOf pkg) ++
                 (getPkgbuildField "makedepends" $ pkgbuildOf pkg)
  (archPkgNames,aurPkgNames,other) <- divideByPkgType depNames
  aurPkgs       <- mapM makeAURPkg aurPkgNames
  recursiveDeps <- mapM (determineDeps lang) aurPkgs
  let (ps,as,os) = foldl groupPkgs (archPkgNames,aurPkgs,other) recursiveDeps
  return (nub ps, nub as, nub os)

-- Note: Make sure to run this on the Virtual Packages first.
mustInstall :: String -> IO Bool
mustInstall pkg = do
  necessaryDeps <- pacmanOutput $ ["-T",pkg]
  return $ length (words necessaryDeps) == 1

-- Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?
getConflicts :: Language -> [String] -> [PacmanPkg] -> [AURPkg] ->
                [VirtualPkg] -> [ErrorMsg]
getConflicts lang toIgnore ps as vs = undefined

getPacmanConflicts :: Language -> [String] -> PacmanPkg -> Maybe String
getPacmanConflicts lang toIgnore pkg
    | isIgnored (pkgNameOf pkg) toIgnore = Just failMessage1
    | '=' `elem` fullName && recVer /= reqVer = Just failMessage2
    | otherwise = Nothing    
    where fullName      = pkgNameWithVersion pkg
          recVer        = getMostRecentVerNum $ pkgInfoOf pkg
          (name,reqVer) = splitNameAndVer fullName
          failMessage1  = getPacmanConflictsMsg2 lang name
          failMessage2  = getPacmanConflictsMsg1 lang name recVer reqVer
       
-- Takes `pacman -Si` output as input
getMostRecentVerNum :: String -> String
getMostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info

-- Strangely identical to `getPacmanConflicts`...
getAURConflicts :: Language -> [String] -> AURPkg -> Maybe String
getAURConflicts lang toIgnore pkg 
    | isIgnored (pkgNameOf pkg) toIgnore = Just failMessage1
    | '=' `elem` fullName && pbVer /= reqVer = Just failMessage2
    | otherwise = Nothing
    where fullName      = pkgNameWithVersion pkg
          pbVer         = head . getPkgbuildField "pkgver" . pkgbuildOf $ pkg
          (name,reqVer) = splitNameAndVer fullName
          failMessage1  = getPacmanConflictsMsg2 lang name
          failMessage2  = getPacmanConflictsMsg1 lang name pbVer reqVer

getVirtualConflicts :: Language -> [String] -> VirtualPkg -> Maybe String
getVirtualConflicts lang toIgnore pkg
    | providerPkgOf pkg == Nothing = Just failMessage1
    | isIgnored provider toIgnore  = Just failMessage2
--    | VERSION CHECKING! is all wrong, by the way.
    | otherwise = Nothing
    where pkgName      = pkgNameOf pkg
          provider     = fromJust $ providerPkgOf pkg
          failMessage1 = getVirtualConflictsMsg1 lang pkgName
          failMessage2 = getVirtualConflictsMsg2 lang pkgName provider

-- Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: Versioning -> String -> Bool
isVersionConflict Anything _    = False
isVersionConflict (MustBe r) c  = not $ c =~ ("^" ++ r)
isVersionConflict (AtLeast r) c | c > r = False
                                | isVersionConflict (MustBe r) c = True
                                | otherwise = False

-- Yields a virtual package's providing package if there is one.
getProvidingPkg :: String -> IO (Maybe String)
getProvidingPkg virt = do
  candidates <- getProvidingPkg' virt
  let lined = lines candidates
  if length lined /= 1  -- No distinct answer? Then fail.
     then return Nothing
     else return . Just . head $ lined

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
getProvidingPkg' :: String -> IO String
getProvidingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  pacmanOutput ["-Ssq",name]
  
isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

isNotInstalled :: String -> IO Bool
isNotInstalled pkg = pacmanFailure ["-Qq",pkg]

isArchPackage :: String -> IO Bool
isArchPackage pkg = pacmanSuccess ["-Si",pkg]

-- A package is an AUR package if it's PKGBUILD exists on the Arch website.
isAURPackage :: String -> IO Bool
isAURPackage = doesUrlExist . getPkgbuildUrl

isVirtualPkg :: String -> IO Bool
isVirtualPkg pkg = do
  provider <- getProvidingPkg pkg
  case provider of
    Just p  -> return True
    Nothing -> return False

splitNameAndVer :: String -> (String,String)
splitNameAndVer pkg = (before,after)
    where (before,_,after) = (pkg =~ "[<>=]+" :: (String,String,String))

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

getInstalledAURPackages :: IO [String]
getInstalledAURPackages = pacmanOutput ["-Qm"] >>= return . lines

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
