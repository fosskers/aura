-- A library for dealing with the installion and management
-- of Arch User Repository packages.

module AuraLib where

-- System Libraries
import System.Directory (renameFile, getCurrentDirectory, setCurrentDirectory)
import Control.Monad (filterM, when, unless)
import Data.List ((\\), nub, sortBy)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.Regex.Posix ((=~))
import Data.Maybe (fromJust)
import Data.Char (isDigit)

-- Custom Libraries
import AuraLanguages
import AurConnection
import Utilities
import Internet
import MakePkg
import Pacman
import Shell

-- For build and package conflict errors.
type ErrMsg = String

-- The global settings as set by the user with command-line flags.
data Settings = Settings { environmentOf   :: Environment
                         , langOf          :: Language
                         , ignoredPkgsOf   :: [String]
                         , cachePathOf     :: FilePath
                         , logFilePathOf   :: FilePath
                         , suppressMakepkg :: Bool
                         , mustConfirm     :: Bool
                         , mayHotEdit      :: Bool
                         }

-----------
-- PACKAGES
-----------
class Package a where
    pkgNameOf :: a -> String
    versionOf :: a -> Versioning

data Versioning = AtLeast String | MustBe String | Anything deriving (Eq,Show)

-- I would like to reduce the following three sets of instance declarations
-- to a single more polymorphic solution.
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

instance Eq PacmanPkg where
    a == b = pkgNameWithVersion a == pkgNameWithVersion b

pkgInfoOf :: PacmanPkg -> String
pkgInfoOf (PacmanPkg _ _ i) = i

-------------------
-- Virtual Packages
-------------------
-- Virtual packages also contain a record of their providing package.
-- Providing packages are assumed to be Pacman (ABS) packages.
-- Are there any instances where this isn't the case?
data VirtualPkg = VirtualPkg String Versioning (Maybe PacmanPkg)

instance Package VirtualPkg where
    pkgNameOf (VirtualPkg n _ _) = n
    versionOf (VirtualPkg _ v _) = v

instance Show VirtualPkg where
    show = pkgNameWithVersion

providerPkgOf :: VirtualPkg -> Maybe PacmanPkg
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
          getVersioning c v | c == ">=" = AtLeast v
                            | c == "="  = MustBe v
                            | otherwise = Anything

-- Constructs anything of the Package type.
makePackage :: Package a => (String -> Versioning -> t -> a) ->
                            (String -> IO t)                 ->
                            String                           ->
                            IO a
makePackage typeCon getThirdField pkg = do
  let (name,ver) = parseNameAndVersioning pkg
  thirdField <- getThirdField name
  return $ typeCon name ver thirdField

makePacmanPkg :: String -> IO PacmanPkg
makePacmanPkg pkg = makePackage PacmanPkg getInfo pkg
    where getInfo name = pacmanOutput ["-Si",name]

makeAURPkg :: String -> IO AURPkg
makeAURPkg pkg = makePackage AURPkg downloadPkgbuild pkg

makeVirtualPkg :: String -> IO VirtualPkg
makeVirtualPkg pkg = makePackage VirtualPkg getProvider pkg
    where getProvider name = do
            providerName <- getProvidingPkg name
            case providerName of
              Nothing  -> return Nothing
              Just pro -> makePacmanPkg pro >>= return . Just

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

-----------
-- The Work
-----------
-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPackageFiles :: [String] -> [FilePath] -> IO ExitCode
installPackageFiles pacOpts files = pacman $ ["-U"] ++ pacOpts ++ files

-- Handles the building of Packages.
-- Assumed: All pacman and AUR dependencies are already installed.
buildPackages :: Settings -> [AURPkg] -> IO [FilePath]
buildPackages _ []            = return []
buildPackages settings (p:ps) = do
  notify settings (flip buildPackagesMsg1 $ pkgNameOf p)
  results <- withTempDir (show p) (build settings p)
  case results of
    Right pkg   -> buildPackages settings ps >>= return . (\pkgs -> pkg : pkgs)
    Left errors -> do        
        toContinue <- buildFail settings p ps errors
        if toContinue
           then return []
           else error . buildPackagesMsg2 . langOf $ settings
        
-- Big and ugly.
-- Perform the actual build. Fails elegantly when build fails occur.
build :: Settings -> AURPkg -> IO (Either String FilePath)
build settings pkg = do
  currDir <- getCurrentDirectory
  allowFullAccess currDir  -- Gives write access to the temp folder.
  tarball   <- downloadSource currDir $ pkgNameOf pkg
  sourceDir <- uncompress tarball
  allowFullAccess sourceDir
  setCurrentDirectory sourceDir
  checkHotEdit settings $ pkgNameOf pkg
  (exitStatus,pkgName,output) <- makepkg' user
  if didProcessFail exitStatus
     then return $ Left output
     else do
       path <- moveToCache (cachePathOf settings) pkgName
       setCurrentDirectory currDir
       return $ Right path
    where makepkg'   = if toSuppress then makepkgQuiet else makepkgVerbose
          toSuppress = suppressMakepkg settings
          user       = getTrueUser $ environmentOf settings

-- Allow the user to edit the PKGBUILD if they asked to do so.
checkHotEdit :: Settings -> String -> IO ()
checkHotEdit settings pkgName = when (mayHotEdit settings) promptForEdit
    where msg    = checkHotEditMsg1 (langOf settings) pkgName
          editor = getEditor $ environmentOf settings
          promptForEdit = do
            answer <- optionalPrompt (mustConfirm settings) msg
            when answer $ openEditor editor "PKGBUILD"

-- Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
-- BUG: This prompting ignores `--noconfirm`.
buildFail :: Settings -> AURPkg -> [AURPkg] -> ErrMsg -> IO Bool
buildFail settings p ps errors = do
  scold settings (flip buildFailMsg1 (show p))
  when (suppressMakepkg settings) (displayBuildErrors settings errors)
  unless (null ps) (scold settings buildFailMsg2 >>
                    mapM_ (putStrLn . colourize cyan . pkgNameOf) ps)
  warn settings buildFailMsg3
  yesNoPrompt (buildFailMsg4 $ langOf settings) "^(y|Y)"

-- If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output. 
displayBuildErrors :: Settings -> ErrMsg -> IO ()
displayBuildErrors settings errors = do
  putStrA red (displayBuildErrorsMsg1 $ langOf settings)
  timedMessage 1000000 ["3.. ","2.. ","1..\n"]
  putStrLn errors

-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> FilePath -> IO FilePath
moveToCache cachePath pkg = renameFile pkg newName >> return newName
    where newName = cachePath </> pkg

-- Returns either a list of error message or the deps to be installed.
getDepsToInstall :: Settings -> [AURPkg] -> 
                    IO (Either [ErrMsg] ([String],[AURPkg]))
getDepsToInstall ss [] = return $ Left [getDepsToInstallMsg1 (langOf ss)]
getDepsToInstall ss pkgs = do
  -- Can this be done with foldM?
  allDeps <- mapM (determineDeps $ langOf ss) pkgs
  let (ps,as,vs) = foldl groupPkgs ([],[],[]) allDeps
  necPacPkgs <- filterM mustInstall ps >>= mapM makePacmanPkg
  necAURPkgs <- filterM (mustInstall . show) as
  necVirPkgs <- filterM mustInstall vs >>= mapM makeVirtualPkg
  let conflicts = getConflicts ss (necPacPkgs,necAURPkgs,necVirPkgs)
  if notNull conflicts
     then return $ Left conflicts
     else do
       let providers  = map (pkgNameOf . fromJust . providerPkgOf) necVirPkgs
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

-- If a package isn't installed, `pacman -T` will yield a single name.
-- Any other type of output means installation is not required. 
mustInstall :: String -> IO Bool
mustInstall pkg = do
  necessaryDeps <- pacmanOutput $ ["-T",pkg]
  return $ length (words necessaryDeps) == 1

-- Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?
getConflicts :: Settings -> ([PacmanPkg],[AURPkg],[VirtualPkg]) -> [ErrMsg]
getConflicts settings (ps,as,vs) = pErr ++ aErr ++ vErr
    where pErr     = clean $ map (getPacmanConflicts lang toIgnore) ps
          aErr     = clean $ map (getAURConflicts lang toIgnore) as
          vErr     = clean $ map (getVirtualConflicts lang toIgnore) vs
          clean    = map fromJust . filter (/= Nothing)
          lang     = langOf settings
          toIgnore = ignoredPkgsOf settings

getPacmanConflicts :: Language -> [String] -> PacmanPkg -> Maybe ErrMsg
getPacmanConflicts lang toIgnore pkg = getRealPkgConflicts f lang toIgnore pkg
    where f = getMostRecentVerNum . pkgInfoOf
       
-- Takes `pacman -Si` output as input.
getMostRecentVerNum :: String -> String
getMostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info

getAURConflicts :: Language -> [String] -> AURPkg -> Maybe ErrMsg
getAURConflicts lang toIgnore pkg = getRealPkgConflicts f lang toIgnore pkg
    where f = getTrueVerViaPkgbuild . pkgbuildOf

-- Must be called with a (f)unction that yields the version number
-- of the most up-to-date form of the package.
getRealPkgConflicts :: Package a => (a -> String) -> Language -> [String] ->
                       a -> Maybe ErrMsg
getRealPkgConflicts f lang toIgnore pkg
    | isIgnored (pkgNameOf pkg) toIgnore       = Just failMessage1
    | isVersionConflict (versionOf pkg) curVer = Just failMessage2
    | otherwise = Nothing    
    where curVer        = f pkg
          (name,reqVer) = splitNameAndVer $ pkgNameWithVersion pkg          
          failMessage1  = getRealPkgConflictsMsg2 lang name
          failMessage2  = getRealPkgConflictsMsg1 lang name curVer reqVer

-- This can't be generalized as easily.
getVirtualConflicts :: Language -> [String] -> VirtualPkg -> Maybe ErrMsg
getVirtualConflicts lang toIgnore pkg
    | providerPkgOf pkg == Nothing = Just failMessage1
    | isIgnored provider toIgnore  = Just failMessage2
    | isVersionConflict (versionOf pkg) pVer = Just failMessage3
    | otherwise = Nothing
    where (name,ver)   = splitNameAndVer $ pkgNameWithVersion pkg
          provider     = pkgNameOf . fromJust . providerPkgOf $ pkg
          pVer         = getProvidedVerNum pkg
          failMessage1 = getVirtualConflictsMsg1 lang name
          failMessage2 = getVirtualConflictsMsg2 lang name provider
          failMessage3 = getVirtualConflictsMsg3 lang name ver provider pVer

getProvidedVerNum :: VirtualPkg -> String
getProvidedVerNum pkg = splitVer match
    where match = info =~ ("[ ]" ++ pkgNameOf pkg ++ ">?=[0-9.]+")
          info  = pkgInfoOf . fromJust . providerPkgOf $ pkg

-- Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: Versioning -> String -> Bool
isVersionConflict Anything _    = False
isVersionConflict (MustBe r) c  = not $ c =~ ("^" ++ r)
isVersionConflict (AtLeast r) c | c > r = False  -- Bug? Same as `-Cs` ordering?
                                | isVersionConflict (MustBe r) c = True
                                | otherwise = False

getInstalledAURPackages :: IO [String]
getInstalledAURPackages = do
  pkgs <- pacmanOutput ["-Qm"]
  return . map fixName . lines $ pkgs
      where fixName = (\(n,v) -> n ++ "=" ++ v) . hardBreak (\c -> c == ' ')

isOutOfDate :: AURPkg -> Bool
isOutOfDate pkg = trueVer == currVer
    where trueVer = getTrueVerViaPkgbuild $ pkgbuildOf pkg
          currVer = case versionOf pkg of
                      MustBe v  -> v
                      AtLeast v -> v
                      Anything  -> ""  -- These might not be appropriate.
  
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

isntAURPackage :: String -> IO Bool
isntAURPackage pkg = isAURPackage pkg >>= notM

-- A package is a virtual package if it has a provider.
isVirtualPkg :: String -> IO Bool
isVirtualPkg pkg = do
  provider <- getProvidingPkg pkg
  case provider of
    Just _  -> return True
    Nothing -> return False

countInstalledPackages :: IO Int
countInstalledPackages = pacmanOutput ["-Qsq"] >>= return . length . lines  

getOrphans :: IO [String]
getOrphans = pacmanOutput ["-Qqdt"] >>= return . lines

removePkgs :: [String] -> [String] -> IO ExitCode
removePkgs [] _         = returnSuccess
removePkgs pkgs pacOpts = pacman $ ["-Rsu"] ++ pkgs ++ pacOpts

-------
-- MISC  -- Too specific for `Utilities.hs`
-------
colouredMessage :: Colour -> Settings -> (Language -> String) -> IO ()
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

-- This could be slow, depending on internet speeds, etc.
divideByPkgType :: [String] -> IO ([String],[String],[String])
divideByPkgType pkgs = do
  archPkgs <- filterM (isArchPackage . splitName) pkgs
  let remaining = pkgs \\ archPkgs
  aurPkgs  <- filterM (isAURPackage . splitName) remaining
  return (archPkgs, aurPkgs, remaining \\ aurPkgs)

sortPkgs :: [String] -> [String]
sortPkgs pkgs = sortBy verNums pkgs
    where verNums a b | nameOf a /= nameOf b = compare a b  -- Different pkgs.
                      | otherwise            = compare (verOf a) (verOf b)
          nameOf = fst . pkgFileNameAndVer
          verOf  = snd . pkgFileNameAndVer

-- linux-3.2.14-1-x86_64.pkg.tar.xz    -> ("linux",[3,2,14,1])
-- wine-1.4rc6-1-x86_64.pkg.tar.xz     -> ("wine",[1,4,6,1])
-- ruby-1.9.3_p125-4-x86_64.pkg.tar.xz -> ("ruby",[1,9,3,125,4])
-- NOTE: regex stuff is a little sloppy here.
pkgFileNameAndVer :: String -> (String,[Int])
pkgFileNameAndVer p = (name,verNum')
    where (name,_,_) = p =~ "-[0-9]+" :: (String,String,String)
          verNum     = p =~ ("[0-9][-0-9a-z._]+-" ++ archs) :: String
          archs      = "(a|x|i)"  -- Representing "(any|x86_64|i686)"
          verNum'    = splitBySubVersion verNum

-- Also discards any non-number version info, like `rc`, etc.
-- Example: "3.2rc6-1" becomes [3,2,6,1]
-- BUG: Explodes if arg doesn't start with a digit.
splitBySubVersion :: String -> [Int]
splitBySubVersion [] = []
splitBySubVersion n  = firstSubVer : splitBySubVersion nextSet
    where firstSubVer = read $ fst digitMatch
          nextSet     = dropWhile (not . isDigit) $ snd digitMatch
          digitMatch  = span isDigit n
