-- Handles all `-C` operations.

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

module Aura.C
    ( downgradable
    , downgradePackages
    , searchCache
    , backupCache
    , cleanCache ) where

import qualified Data.Set as S

import System.Posix.Files (fileExist)
import Data.List ((\\),sort,groupBy)
import System.FilePath ((</>))
import Text.Regex.PCRE ((=~))
import Control.Monad (filterM)
import System.Exit (ExitCode)
import Data.Char (isDigit)

import Aura.Pacman (cacheContents)
import Aura.Logo (raiseCursorBy)
import Aura.Languages
import Aura.Settings
import Aura.General

import Shell (rm, cp, returnSuccess)
import Zero ((?>>))
import Utilities

---

type Cache   = S.Set PkgFile
type PkgFile = String

-- What's the best form for the Cache to be in?
-- Probably one that allows us to use the `member` function.
downgradable :: Cache -> String -> Bool
downgradable c s = undefined

-- Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: Settings -> [String] -> IO ExitCode
downgradePackages _ []    = returnSuccess
downgradePackages ss pkgs = do
  reals <- filterM isInstalled pkgs
  reportBadDowngradePkgs (langOf ss) (pkgs \\ reals)
  return reals ?>> do
    cache   <- cacheContents cachePath
    choices <- mapM (getDowngradeChoice ss cache) reals
    pacman ss $ ["-U"] ++ map (cachePath </>) choices
      where cachePath = cachePathOf ss
               
getDowngradeChoice :: Settings -> [PkgFile] -> String -> IO String
getDowngradeChoice settings cache pkg = do
  let choices = getChoicesFromCache cache pkg
  notify settings (flip getDowngradeChoiceMsg1 pkg)
  getSelection choices

getChoicesFromCache :: [PkgFile] -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter (\p -> p =~ ("^" ++ pkg ++ "-[0-9]")) cache

-- `[]` as input yields the contents of the entire cache.
searchCache :: Settings -> [String] -> IO ExitCode
searchCache settings input = do
  cache <- cacheContents $ cachePathOf settings  
  mapM_ putStrLn . sortPkgs . searchLines (unwords input) $ cache
  returnSuccess

-- The destination folder must already exist for the back-up to being.
backupCache :: Settings -> [String] -> IO ExitCode
backupCache settings []      = scoldAndFail settings backupCacheMsg1
backupCache settings (dir:_) = do
  exists <- fileExist dir
  if not exists
     then scoldAndFail settings backupCacheMsg3
     else do
       cache <- cacheContents $ cachePathOf settings
       notify settings $ flip backupCacheMsg4 dir
       notify settings . flip backupCacheMsg5 . length $ cache
       okay <- optionalPrompt (mustConfirm settings)
               (backupCacheMsg6 $ langOf settings)
       if not okay
          then scoldAndFail settings backupCacheMsg7
          else do
            notify settings backupCacheMsg8
            putStrLn ""  -- So that the cursor can rise at first.
            copyAndNotify settings dir cache 1

-- Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: Settings -> FilePath -> [String] -> Int -> IO ExitCode
copyAndNotify _ _ [] _              = returnSuccess
copyAndNotify settings dir (p:ps) n = do
  putStr $ raiseCursorBy 1
  warn settings (flip copyAndNotifyMsg1 n)
  cp (cachePath </> p) (dir </> p)
  copyAndNotify settings dir ps $ n + 1
      where cachePath = cachePathOf settings

-- Acts as a filter for the input to `cleanCache`.
cleanCache :: Settings -> [String] -> IO ExitCode
cleanCache settings [] = cleanCache' settings 0
cleanCache settings (input:_)  -- Ignores all but first input element.
  | all isDigit input = cleanCache' settings $ read input
  | otherwise         = scoldAndFail settings $ flip preCleanCacheMsg1 input

-- Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache' :: Settings -> Int -> IO ExitCode
cleanCache' ss toSave
    | toSave < 0  = scoldAndFail ss cleanCacheMsg1
    | toSave == 0 = warn ss cleanCacheMsg2 >> pacman ss ["-Scc"]
    | otherwise   = do
        warn ss $ flip cleanCacheMsg3 toSave
        okay <- optionalPrompt (mustConfirm ss) (cleanCacheMsg4 $ langOf ss)
        if not okay
           then scoldAndFail ss cleanCacheMsg5
           else do
             notify ss cleanCacheMsg6
             cache <- cacheContents $ cachePathOf ss
             let grouped = map (take toSave . reverse) $ groupByPkgName cache
                 toRemove  = cache \\ concat grouped
                 filePaths = map (cachePathOf ss </>) toRemove
             mapM_ rm filePaths  -- Error handling?
             returnSuccess

-- Typically takes the contents of the package cache as an argument.
groupByPkgName :: [String] -> [[String]]
groupByPkgName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = tripleFst (p =~ "-[0-9]+" :: (String,String,String))

------------
-- REPORTING
------------
reportBadDowngradePkgs :: Language -> [String] -> IO ()
reportBadDowngradePkgs lang ps = badReport reportBadDowngradePkgsMsg1 lang ps