{-# LANGUAGE MultiWayIf #-}

-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

import System.Directory (doesFileExist)
import Text.Regex.PCRE  ((=~))
import System.IO        (hFlush, stdout)

import Aura.Settings.Base (pacmanCmdOf)
import Aura.Languages     (pacmanFailure_1)
import Aura.Shell         (shellCmd, quietShellCmd, quietShellCmd')
import Aura.Utils         (scoldAndFail)
import Aura.Monad.Aura
import Aura.Cache

import Shell (Environment, getEnvVar, didProcessSucceed)
import Utilities 

---

type ShellArg = String

defaultCmd :: String
defaultCmd = "pacman"

powerPillCmd :: String
powerPillCmd = "/usr/bin/powerpill"

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

lockFile :: FilePath
lockFile = "/var/lib/pacman/db.lck"

getPacmanCmd :: Environment -> Bool -> IO String
getPacmanCmd env nopp =
    case getEnvVar "PACMAN" env of
      Just cmd -> return cmd
      Nothing  -> do  -- Left space for more options later.
        powerPill <- doesFileExist powerPillCmd
        if | powerPill && not nopp -> return powerPillCmd
           | otherwise -> return defaultCmd

getPacmanConf :: IO String
getPacmanConf = readFileUTF8 pacmanConfFile

getConfFileField :: String -> String -> [String]
getConfFileField confFile field = words $ takeWhile (/= '\n') entry
    where (_,_,entry) = confFile =~ field :: (String,String,String)

getIgnoredPkgs :: String -> [String]
getIgnoredPkgs confFile = getConfFileField confFile "^IgnorePkg[ ]+=[ ]+"

-- For config file fields that only have one value.
-- Caller must supply an alternative if the given field isn't found.
singleEntry :: String -> String -> String -> String
singleEntry confFile field alt = case getConfFileField confFile regex of
                                      []    -> alt
                                      entry -> noQs $ head entry
    where regex = "^" ++ field ++ "[ ]*=[ ]*"
          noQs  = filter (`notElem` "\"")

getCachePath :: String -> FilePath
getCachePath confFile = singleEntry confFile "CacheDir" defaultPackageCache

getLogFilePath :: String -> FilePath
getLogFilePath confFile = singleEntry confFile "LogFile" defaultLogFile

----------
-- ACTIONS
----------
pacman :: [ShellArg] -> Aura ()
pacman args = asks pacmanCmdOf >>= \cmd -> flush >> shellCmd cmd args
    where flush = liftIO (hFlush stdout)

-- Did a pacman process succeed?
pacmanSuccess :: [ShellArg] -> Aura Bool
pacmanSuccess args = success <$> quietShellCmd' "pacman" args
    where success = didProcessSucceed . tripleFst

-- Handler for pacman call failures.
pacmanFailure :: String -> Aura a
pacmanFailure _ = scoldAndFail pacmanFailure_1

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [ShellArg] -> Aura String
pacmanOutput args = quietShellCmd "pacman" args

syncDatabase :: [ShellArg] -> Aura ()
syncDatabase pacOpts = pacman $ ["-Sy"] ++ pacOpts

getPacmanHelpMsg :: Aura [String]
getPacmanHelpMsg = lines <$> pacmanOutput ["-h"]

-- Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: Aura [String]
getVersionInfo = (map (drop verMsgPad) . lines) <$> pacmanOutput ["-V"]

-- The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
