-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

-- System Libraries
import System.Process (readProcessWithExitCode, rawSystem)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Regex.Posix ((=~))

-- Custom Libraries
import AuraLanguages
import Utilities 

type Arg = String

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultPackageCache :: FilePath
defaultPackageCache = "/var/cache/pacman/pkg/"

pacman :: [Arg] -> IO ()
pacman args = hFlush stdout >> rawSystem "pacman" args >> return ()

-- Runs pacman without producing any output.
pacmanQuiet :: [Arg] -> IO (ExitCode,String,String)
pacmanQuiet args = readProcessWithExitCode "pacman" args ""

-- Did a pacman process succeed?
pacmanSuccess :: [Arg] -> IO Bool
pacmanSuccess args = do
  (exitStatus,_,_) <- pacmanQuiet args
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False

pacmanFailure :: [Arg] -> IO Bool
pacmanFailure args = pacmanSuccess args >>= return . not

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [Arg] -> IO String
pacmanOutput args = pacmanQuiet args >>= return . tripleSnd

syncDatabase :: Language -> IO ()
syncDatabase lang = do
  putStrLnA green $ syncDatabaseMsg1 lang
  pacman ["-Sy"]

-- This takes the filepath of the package cache as an argument.
packageCacheContents :: FilePath -> IO [String]
packageCacheContents = getDirectoryContents

getPacmanConf :: IO String
getPacmanConf = readFile pacmanConfFile

getConfFileField :: String -> String -> [String]
getConfFileField confFile field = words $ takeWhile (not . (==) '\n') entry
    where (_,_,entry) = confFile =~ field :: (String,String,String)

getIgnoredPkgs :: String -> [String]
getIgnoredPkgs confFile = getConfFileField confFile "^IgnorePkg[ ]+=[ ]+"

getCachePath :: String -> FilePath
getCachePath confFile = case getConfFileField confFile pattern of
                          []    -> defaultPackageCache
                          entry -> head entry
    where pattern = "^CacheDir[ ]+=[ ]+"

getPacmanHelpMsg :: IO [String]
getPacmanHelpMsg = do
  helpMsg <- pacmanOutput ["-h"]
  return $ lines helpMsg

-- Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: IO [String]
getVersionInfo = do
  versionLines <- pacmanOutput ["-V"]
  return . map (drop lineHeaderLength) . lines $ versionLines

-- The amount of whitespace before text in the lines given by `pacman -V`
lineHeaderLength :: Int
lineHeaderLength = 23

