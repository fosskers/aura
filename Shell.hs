-- An abstraction layer for shell-related tasks.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

{- CITATION

`Escape Codes` section is directly borrowed from:
  library: ansi-terminal
  author:  Max Bolingbroke
  contact: <batterseapower@hotmail.com>

-}

module Shell where

-- System Libraries
import System.Process (readProcess, readProcessWithExitCode, rawSystem)
import System.Exit (ExitCode(..))
import Data.List (intersperse)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import System.Directory ( getDirectoryContents
                        , setCurrentDirectory
                        , getCurrentDirectory
                        , removeFile
                        , renameFile
                        , copyFile )

-- Custom Libraries
import Zero

----------------------
-- SYSTEM CALL ALIASES
----------------------
pwd :: IO String
pwd = getCurrentDirectory

rm :: FilePath -> IO ()
rm = removeFile

ls :: FilePath -> IO [FilePath]
ls = getDirectoryContents

-- Would this work?
-- drop 2 `liftM` getDirectoryContents
ls' :: FilePath -> IO [FilePath]
ls' p = noDots `liftM` ls p
    where noDots = filter (`notElem` [".",".."])

mv :: FilePath -> FilePath -> IO ()
mv = renameFile

cd :: FilePath -> IO ()
cd = setCurrentDirectory

cp :: FilePath -> FilePath -> IO ()
cp = copyFile

chown :: String -> FilePath -> [String] -> IO ()
chown user path args = do
  _ <- quietShellCmd "chown" (args ++ [user,path])
  return ()

---------------
-- ESCAPE CODES
---------------
-- Code borrowed from `ansi-terminal` library by Max Bolingbroke.
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

cursorUpLineCode :: Int -> String
cursorUpLineCode n = csi [n] "F"

hideCursor :: IO ()
hideCursor = putStr hideCursorCode

showCursor :: IO ()
showCursor = putStr showCursorCode

hideCursorCode :: String
hideCursorCode = csi [] "?25l"

showCursorCode :: String
showCursorCode = csi [] "?25h"

---------------
-- SYSTEM CALLS
---------------
-- Calls a child process that suspends the current one and takes over.
shellCmd :: String -> [String] -> IO ExitCode
shellCmd = rawSystem

-- Suppresses output, but returns it on completion.
quietShellCmd :: String -> [String] -> IO String
quietShellCmd cmd args = readProcess cmd args ""

-- Return type is slightly more verbose than `quietShellCmd`.
quietShellCmd' :: String -> [String] -> IO (ExitCode,String,String)
quietShellCmd' cmd args = readProcessWithExitCode cmd args ""

-------------
-- EXIT CODES
-------------
didProcessSucceed :: ExitCode -> Bool
didProcessSucceed ExitSuccess = True
didProcessSucceed _           = False

didProcessFail :: ExitCode -> Bool
didProcessFail = not . didProcessSucceed

returnSuccess :: IO ExitCode
returnSuccess = return ExitSuccess

returnFailure :: IO ExitCode
returnFailure = return zero

------------------------
-- ENVIRONMENT VARIABLES
------------------------
type Environment = [(String,String)]

getEnvVar :: String -> Environment -> Maybe String
getEnvVar v = lookup v

varExists :: String -> Environment -> Bool
varExists v env = case getEnvVar v env of
                    Just _  -> True
                    Nothing -> False

-- As of `sudo 1.8.6`, the USER variable disappears when using `sudo`.
getUser :: Environment -> Maybe String
getUser = getEnvVar "USER"

-- I live on the edge.
getUser' :: Environment -> String
getUser' = fromJust . getUser

-- This variable won't exist if the current program wasn't run with `sudo`.
getSudoUser :: Environment -> Maybe String
getSudoUser = getEnvVar "SUDO_USER"

getSudoUser' :: Environment -> String
getSudoUser' = fromJust . getSudoUser

-- Is the user root, or using sudo?
hasRootPriv :: Environment -> Bool
hasRootPriv env = varExists "SUDO_USER" env || isTrueRoot env

isTrueRoot :: Environment -> Bool
isTrueRoot env = varExists "USER" env   &&
                 getUser' env == "root" &&
                 not (varExists "SUDO_USER" env)

isntTrueRoot :: Environment -> Bool
isntTrueRoot = not . isTrueRoot

-- This will get the true user name regardless of sudo-ing.
getTrueUser :: Environment -> String
getTrueUser env | isTrueRoot env  = "root"
                | hasRootPriv env = getSudoUser' env
                | otherwise       = getUser' env

getEditor :: Environment -> String
getEditor env = case getEnvVar "EDITOR" env of
                  Just emacs -> emacs  -- ;)
                  Nothing    -> "vi"   -- `vi` should be available.
