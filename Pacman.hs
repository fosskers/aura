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

defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

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

syncDatabase :: Language -> [String] -> IO ()
syncDatabase lang pacOpts = do
  putStrLnA green $ syncDatabaseMsg1 lang
  pacman $ ["-Sy"] ++ pacOpts

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

-- For config file fields that only have one value.
-- Caller must supply an alternative if the given field isn't found.
getSingleEntry :: String -> String -> String -> String
getSingleEntry confFile field alt = case getConfFileField confFile regex of
                                      []    -> alt
                                      entry -> head entry
    where regex = "^" ++ field ++ "[ ]+=[ ]+"

getCachePath :: String -> FilePath
getCachePath confFile = getSingleEntry confFile "CacheDir" defaultPackageCache

getLogFilePath :: String -> FilePath
getLogFilePath confFile = getSingleEntry confFile "LogFile" defaultLogFile

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
