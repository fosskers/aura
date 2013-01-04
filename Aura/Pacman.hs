-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Pacman where

-- System Libraries
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Regex.PCRE ((=~))
import Control.Monad (liftM)

-- Custom Libraries
import Aura.Cache
import Utilities 
import Shell

type ShellArg = String
type Pacman   = [ShellArg] -> IO ExitCode

defaultCmd :: String
defaultCmd = "pacman"

pacmanColorCmd :: String
pacmanColorCmd = "pacman-color"

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

pacmanCmd :: String -> [ShellArg] -> IO ExitCode
pacmanCmd cmd args = hFlush stdout >> shellCmd cmd args

-- Runs pacman without producing any output.
pacmanQuiet :: [ShellArg] -> IO (ExitCode,String,String)
pacmanQuiet args = quietShellCmd' "pacman" args

-- Did a pacman process succeed?
pacmanSuccess :: [ShellArg] -> IO Bool
pacmanSuccess args = (didProcessSucceed . tripleFst) `liftM` pacmanQuiet args

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [ShellArg] -> IO String
pacmanOutput args = tripleSnd `liftM` pacmanQuiet args

syncDatabase :: Pacman -> [ShellArg] -> IO ExitCode
syncDatabase pacman pacOpts = pacman $ ["-Sy"] ++ pacOpts

-- I'm sad that I had to make this Monadic. And a lot uglier.
getPacmanCmd :: Environment -> IO Pacman
getPacmanCmd env = pacmanCmd `liftM` getPacmanCmd' env

getPacmanCmd' :: Environment -> IO String
getPacmanCmd' env = case getEnvVar "PACMAN" env of
                     Just cmd -> return cmd
                     Nothing  -> do
                       installed <- doesFileExist pacmanColorCmd
                       if installed
                          then return pacmanColorCmd
                          else return defaultCmd

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
