-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

-- System Libraries
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Regex.Posix ((=~))
import Control.Monad (liftM)

-- Custom Libraries
import Utilities 
import Shell

type Arg = String

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultPackageCache :: FilePath
defaultPackageCache = "/var/cache/pacman/pkg/"

defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

pacman :: [Arg] -> IO ExitCode
pacman args = hFlush stdout >> shellCmd "pacman" args

-- Slight evil-doing permitted here.
pacman' :: [Arg] -> IO ()
pacman' args = pacman args >> return ()

-- Runs pacman without producing any output.
pacmanQuiet :: [Arg] -> IO (ExitCode,String,String)
pacmanQuiet args = quietShellCmd' "pacman" args

-- Did a pacman process succeed?
pacmanSuccess :: [Arg] -> IO Bool
pacmanSuccess args = (didProcessSucceed . tripleFst) `liftM` pacmanQuiet args

pacmanFailure :: [Arg] -> IO Bool
pacmanFailure args = not `liftM` pacmanSuccess args

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [Arg] -> IO String
pacmanOutput args = tripleSnd `liftM` pacmanQuiet args

syncDatabase :: [Arg] -> IO ExitCode
syncDatabase pacOpts = pacman $ ["-Sy"] ++ pacOpts

-- This takes the filepath of the package cache as an argument.
packageCacheContents :: FilePath -> IO [String]
packageCacheContents c = filter dots `liftM` getDirectoryContents c
    where dots p = p `notElem` [".",".."]

getPacmanConf :: IO String
getPacmanConf = readFile pacmanConfFile

getConfFileField :: String -> String -> [String]
getConfFileField confFile field = words $ takeWhile (/= '\n') entry
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
getPacmanHelpMsg = lines `liftM` pacmanOutput ["-h"] 

-- Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: IO [String]
getVersionInfo = (map (drop verMsgPad) . lines) `liftM` pacmanOutput ["-V"]

-- The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
