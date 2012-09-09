-- A library for dealing with the installion and management
-- of Arch User Repository packages.

module AuraLib where

-- System Libraries
import System.Directory (renameFile, getCurrentDirectory, setCurrentDirectory)
import Control.Monad (filterM, liftM, when, unless)
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
import Zero

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
makePackage typeCon getThirdField pkg = do
  let (name,ver) = parseNameAndVersionDemand pkg
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
              Just pro -> Just `liftM` makePacmanPkg pro

-- Yields a virtual package's providing package if there is one.
getProvidingPkg :: String -> IO (Maybe String)
getProvidingPkg virt = do
  candidates <- getProvidingPkg' virt
  let lined = lines candidates
  (length lined /= 1) ?>> (return . Just . head $ lined)

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
getProvidingPkg' :: String -> IO String
getProvidingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  pacmanOutput ["-Ssq",name]

------------
-- OPERATORS
------------
-- Fails immediately if a certain predicate is not met.
(?>>) :: (Eq a, Zero a, Eq b, Zero b, Monad m) => a -> m b -> m b
val ?>> action | val == zero = return zero
               | otherwise   = action

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

---------------
-- ABSTRACTIONS
---------------
{-
type PkgMap = (Eq a) =>
              (a -> IO Bool)                 -- A predicate for filtering.
              -> (Language -> [a] -> IO ())  -- Notify of filtered packages.
              -> (a -> IO b)                 -- Action to perform.
              -> Settings
              -> [a]                         -- Packages.
-}
--mapOverPkgs :: PkgMap -> IO [a]               
mapOverPkgs cond report fun settings pkgs = do
  realPkgs <- filterM cond pkgs
  _ <- report (langOf settings) $ pkgs \\ realPkgs
  mapM fun realPkgs

--mapOverPkgs' :: PkgMap -> IO ExitCode
mapOverPkgs' cond report fun settings pkgs = do
  result <- mapOverPkgs cond report fun settings pkgs
  notNull result ?>> returnSuccess

-----------
-- THE WORK
-----------
-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPackageFiles :: [String] -> [FilePath] -> IO ExitCode
installPackageFiles pacOpts files = pacman $ ["-U"] ++ pacOpts ++ files

-- Handles the building of Packages.
-- Assumed: All pacman and AUR dependencies are already installed.
buildPackages :: Settings -> [AURPkg] -> IO (Maybe [FilePath])
buildPackages _ [] = return $ Just []  -- Done recursing!
buildPackages settings pkgs@(p:ps) = do
  notify settings (flip buildPackagesMsg1 $ pkgNameOf p)
  results <- withTempDir (show p) (build settings p)
  case results of
    Right pkg   -> (fmap (pkg :)) `liftM` buildPackages settings ps
    Left errors -> buildFail settings pkgs errors
        
-- Big and ugly.
-- Perform the actual build. Fails elegantly when build fails occur.
build :: Settings -> AURPkg -> IO (Either ErrMsg FilePath)
build settings pkg = do
  currDir <- getCurrentDirectory
  getSourceCode (pkgNameOf pkg) user currDir
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

getSourceCode :: String -> String -> FilePath -> IO ()
getSourceCode pkgName user currDir = do
  chown user currDir []
  tarball   <- downloadSource currDir pkgName
  sourceDir <- uncompress tarball
  chown user sourceDir ["-R"]
  setCurrentDirectory sourceDir

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
buildFail :: Settings -> [AURPkg] -> ErrMsg -> IO (Maybe [FilePath])
buildFail _ [] _ = return Nothing
buildFail settings (p:ps) errors = do
  scold settings (flip buildFailMsg1 (show p))
  when (suppressMakepkg settings) (displayBuildErrors settings errors)
  unless (null ps) (scold settings buildFailMsg2 >>
                    mapM_ (putStrLn . cyan . pkgNameOf) ps)
  warn settings buildFailMsg3
  toContinue <- yesNoPrompt (buildFailMsg4 $ langOf settings) "^(y|Y)"
  toContinue ?>> (return $ Just [])

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
    where pErr     = extract $ map (getPacmanConflicts lang toIgnore) ps
          aErr     = extract $ map (getAURConflicts lang toIgnore) as
          vErr     = extract $ map (getVirtualConflicts lang toIgnore) vs
          extract  = map fromJust . filter (/= Nothing)
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
          (name,reqVer) = splitNameAndVer $ pkgNameWithVersionDemand pkg
          failMessage1  = getRealPkgConflictsMsg2 lang name
          failMessage2  = getRealPkgConflictsMsg1 lang name curVer reqVer

-- This can't be generalized as easily.
getVirtualConflicts :: Language -> [String] -> VirtualPkg -> Maybe ErrMsg
getVirtualConflicts lang toIgnore pkg
    | providerPkgOf pkg == Nothing = Just failMessage1
    | isIgnored provider toIgnore  = Just failMessage2
    | isVersionConflict (versionOf pkg) pVer = Just failMessage3
    | otherwise = Nothing
    where (name,ver)   = splitNameAndVer $ pkgNameWithVersionDemand pkg
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
isVersionConflict :: VersionDemand -> String -> Bool
isVersionConflict Anything _     = False
isVersionConflict (LessThan r) c = not $ comparableVer c < comparableVer r
isVersionConflict (MustBe r) c   = not $ c =~ ("^" ++ r)
isVersionConflict (AtLeast r) c  | comparableVer c > comparableVer r = False
                                 | isVersionConflict (MustBe r) c = True
                                 | otherwise = False

getForeignPackages :: IO [(String,String)]
getForeignPackages = do
  pkgs <- lines `liftM` pacmanOutput ["-Qm"]
  return $ map fixName pkgs
      where fixName = hardBreak (== ' ')

isOutOfDate :: (PkgInfo,String) -> Bool
isOutOfDate (info,v) = trueVer > currVer
  where trueVer = comparableVer $ latestVerOf info
        currVer = comparableVer v

isIgnored :: String -> [String] -> Bool
isIgnored pkg toIgnore = pkg `elem` toIgnore

isInstalled :: String -> IO Bool
isInstalled pkg = pacmanSuccess ["-Qq",pkg]

isNotInstalled :: String -> IO Bool
isNotInstalled pkg = pacmanFailure ["-Qq",pkg]

isABSPkg :: String -> IO Bool
isABSPkg pkg = pacmanSuccess ["-Si",pkg]

-- A package is an AUR package if it's PKGBUILD exists on the Arch website.
-- Requires internet access.
isAURPkg :: String -> IO Bool
isAURPkg = doesUrlExist . getPkgbuildUrl

isntAURPkg :: String -> IO Bool
isntAURPkg pkg = not `liftM` isAURPkg pkg

-- A package is a virtual package if it has a provider.
isVirtualPkg :: String -> IO Bool
isVirtualPkg pkg = do
  provider <- getProvidingPkg pkg
  provider ?>> return True

countInstalledPackages :: IO Int
countInstalledPackages = (length . lines) `liftM` pacmanOutput ["-Qsq"]

getOrphans :: IO [String]
getOrphans = lines `liftM` pacmanOutput ["-Qqdt"]

removePkgs :: [String] -> [String] -> IO ExitCode
removePkgs [] _         = returnSuccess
removePkgs pkgs pacOpts = pacman $ ["-Rsu"] ++ pkgs ++ pacOpts

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

-- This could be slow, depending on internet speeds, etc.
divideByPkgType :: [String] -> IO ([String],[String],[String])
divideByPkgType pkgs = do
  archPkgs <- filterM (isABSPkg . splitName) pkgs
  let remaining = pkgs \\ archPkgs
  aurPkgs  <- filterM (isAURPkg . splitName) remaining
  return (archPkgs, aurPkgs, remaining \\ aurPkgs)

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
