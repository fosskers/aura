-- Handles all `-C` operations.

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

module Aura.Commands.C
    ( downgradePackages
    , searchCache
    , backupCache
    , cleanCache ) where

import System.Posix.Files (fileExist)
import Data.List ((\\),sort,groupBy)
import System.FilePath ((</>))
import Text.Regex.PCRE ((=~))
import Control.Monad (filterM)
import Control.Monad (liftM)
import Data.Char (isDigit)

import Aura.Logo (raiseCursorBy)
import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.General
import Aura.Cache
import Aura.Utils

import Shell (rm, cp)
import Zero ((?>>))
import Utilities

---

-- Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: [String] -> Aura ()
downgradePackages []   = return ()
downgradePackages pkgs = do
  reals <- filterM isInstalled pkgs
  reportBadDowngradePkgs (pkgs \\ reals)
  cachePath <- cachePathOf `liftM` ask
  if null reals
     then return ()
     else do
       cache   <- cacheContents cachePath
       choices <- mapM (getDowngradeChoice cache) reals
       pacman $ ["-U"] ++ map (cachePath </>) choices
               
getDowngradeChoice :: Cache -> String -> Aura String
getDowngradeChoice cache pkg = do
  let choices = getChoicesFromCache cache pkg
  notify (flip getDowngradeChoiceMsg1 pkg)
  liftIO $ getSelection choices

getChoicesFromCache :: Cache -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter match (allFilenames cache)
          match p = p =~ ("^" ++ pkg ++ "-[0-9]")

-- `[]` as input yields the contents of the entire cache.
searchCache :: [String] -> Aura ()
searchCache input = do
  cache <- ask >>= cacheContents . cachePathOf
  let results = sortPkgs . searchLines (unwords input) . allFilenames $ cache
  liftIO $ mapM_ putStrLn results

-- The destination folder must already exist for the back-up to being.
backupCache :: [String] -> Aura ()
backupCache []      = scoldAndFail backupCacheMsg1
backupCache (dir:_) = ask >>= \ss -> do
  exists <- liftIO $ fileExist dir
  if not exists
     then scoldAndFail backupCacheMsg3
     else do
       cache <- cacheContents $ cachePathOf ss
       notify $ flip backupCacheMsg4 dir
       notify . flip backupCacheMsg5 . size $ cache
       okay <- optionalPrompt (backupCacheMsg6 $ langOf ss)
       if not okay
          then scoldAndFail backupCacheMsg7
          else do
            notify backupCacheMsg8
            liftIO $ putStrLn ""  -- So that the cursor can rise at first.
            copyAndNotify dir (allFilenames cache) 1

-- Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: FilePath -> [String] -> Int -> Aura ()
copyAndNotify _ [] _       = return ()
copyAndNotify dir (p:ps) n = do
  cachePath <- cachePathOf `liftM` ask
  liftIO $ putStr $ raiseCursorBy 1
  warn (flip copyAndNotifyMsg1 n)
  liftIO $ cp (cachePath </> p) (dir </> p)
  copyAndNotify dir ps $ n + 1

-- Acts as a filter for the input to `cleanCache`.
cleanCache :: [String] -> Aura ()
cleanCache [] = cleanCache' 0
cleanCache (input:_)  -- Ignores all but first input element.
  | all isDigit input = cleanCache' $ read input
  | otherwise         = scoldAndFail $ flip preCleanCacheMsg1 input

-- Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache' :: Int -> Aura ()
cleanCache' toSave
    | toSave < 0  = scoldAndFail cleanCacheMsg1
    | toSave == 0 = warn cleanCacheMsg2 >> pacman ["-Scc"]
    | otherwise   = ask >>= \ss -> do
        warn $ flip cleanCacheMsg3 toSave
        okay <- optionalPrompt (cleanCacheMsg4 $ langOf ss)
        if not okay
           then scoldAndFail cleanCacheMsg5
           else do
             notify cleanCacheMsg6
             cache <- cacheContents $ cachePathOf ss
             let files     = allFilenames cache
                 grouped   = map (take toSave . reverse) $ groupByName files
                 toRemove  = files \\ concat grouped
                 filePaths = map (cachePathOf ss </>) toRemove
             liftIO $ mapM_ rm filePaths

-- Typically takes the contents of the package cache as an argument.
groupByName :: [String] -> [[String]]
groupByName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = tripleFst (p =~ "-[0-9]+" :: (String,String,String))

------------
-- REPORTING
------------
reportBadDowngradePkgs :: [String] -> Aura ()
reportBadDowngradePkgs ps = badReport reportBadDowngradePkgsMsg1 ps
