{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Handles all `-C` operations.

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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
  , cleanCache
  , cleanNotSaved
  ) where

import           Aura.Cache
import           Aura.Core
import           Aura.Languages
import           Aura.Logo (raiseCursorBy)
import           Aura.Monad.Aura
import           Aura.Pacman (pacman)
import           Aura.Settings.Base
import           Aura.State
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly hiding (path)
import           Text.Regex.PCRE ((=~))
import           Utilities

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: [T.Text] -> [T.Text] -> Aura ()
downgradePackages _ []         = pure ()
downgradePackages pacOpts pkgs = do
  ss        <- ask
  let cachePath = cachePathOf ss
  reals     <- shelly $ pkgsInCache ss pkgs
  reportBadDowngradePkgs . map T.unpack $ pkgs \\ reals
  unless (null reals) $ do
    cache   <- shelly $ cacheContents cachePath
    choices <- traverse (fmap T.pack . getDowngradeChoice cache) $ map T.unpack reals
    pacman $ "-U" : pacOpts <> (map (toTextIgnore . (cachePath </>)) choices)

getDowngradeChoice :: Cache -> String -> Aura String
getDowngradeChoice cache pkg = do
  let choices = getChoicesFromCache cache pkg
  asks langOf >>= notify . getDowngradeChoice_1 pkg
  liftIO $ getSelection choices

-- TODO Could use a real parser for this, and keep `Text`.
getChoicesFromCache :: Cache -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter match (map T.unpack $ M.elems cache)
          patt    = "-[0-9:.]+-[1-9]+-(x86_64|i686|any)[.]pkg[.]tar[.]xz$"
          match p = p =~ ("^" <> pkg <> patt)

-- `[]` as input yields the contents of the entire cache.
searchCache :: [T.Text] -> Aura ()
searchCache ps = do
  ss <- ask
  matches <- shelly $ cacheMatches ss ps
  liftIO . traverse_ putStrLn . sortPkgs $ map T.unpack matches

-- The destination folder must already exist for the back-up to begin.
backupCache :: [FilePath] -> Aura ()
backupCache []      = scoldAndFail backupCache_1
backupCache (dir:_) = do
  exists <- shelly $ test_d dir
  if not exists
     then scoldAndFail backupCache_3
     else confirmBackup dir >>= backup dir

confirmBackup :: FilePath -> Aura Cache
confirmBackup dir = do
  ss    <- ask
  cache <- shelly . cacheContents $ cachePathOf ss
  notify $ backupCache_4 (T.unpack $ toTextIgnore dir) (langOf ss)
  notify $ backupCache_5 (M.size cache) (langOf ss)
  okay  <- optionalPrompt ss backupCache_6
  if not okay
     then scoldAndFail backupCache_7
     else pure cache

backup :: FilePath -> Cache -> Aura ()
backup dir cache = do
  asks langOf >>= notify . backupCache_8
  liftIO $ putStrLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: FilePath -> [T.Text] -> Int -> Aura ()
copyAndNotify _ [] _       = pure ()
copyAndNotify dir (p:ps) n = asks cachePathOf >>= \cachePath -> do
  liftIO $ raiseCursorBy 1
  asks langOf >>= warn . copyAndNotify_1 n
  shelly $ cp (cachePath </> p) (dir </> p)
  copyAndNotify dir ps $ n + 1

-- Acts as a filter for the input to `cleanCache`.
cleanCache :: [String] -> Aura ()
cleanCache [] = cleanCache' 0
cleanCache (input:_)  -- Ignores all but first input element.
  | all isDigit input = cleanCache' $ read input
  | otherwise         = scoldAndFail $ preCleanCache_1 input

-- Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache' :: Int -> Aura ()
cleanCache' toSave
    | toSave < 0  = scoldAndFail cleanCache_1
    | toSave == 0 = asks langOf >>= warn . cleanCache_2 >> pacman ["-Scc"]  -- Needed?
    | otherwise   = do
        ss <- ask
        warn . cleanCache_3 toSave $ langOf ss
        okay <- optionalPrompt ss cleanCache_4
        if not okay
           then scoldAndFail cleanCache_5
           else clean toSave

clean :: Int -> Aura ()
clean toSave = ask >>= \ss -> do
  notify . cleanCache_6 $ langOf ss
  cache <- shelly . cacheContents $ cachePathOf ss
  let files     = map T.unpack $ M.elems cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files \\ fold grouped
      filePaths = (cachePathOf ss </>) <$> toRemove
  shelly $ traverse_ rm filePaths

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: Aura ()
cleanNotSaved = do
  ss     <- ask
  notify . cleanNotSaved_1 $ langOf ss
  states <- getStateFiles >>= liftIO . traverse readState
  let path = cachePathOf ss
  cache  <- shelly $ cacheContents path
  let duds = M.filterWithKey (\p _ -> or (map (inState p) states)) cache
  whenM (optionalPrompt ss $ cleanNotSaved_2 $ M.size duds) $
        shelly $ traverse_ rm (map (path </>) $ M.elems duds)

-- Typically takes the contents of the package cache as an argument.
groupByName :: [String] -> [[String]]
groupByName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = tripleFst (p =~ ("-[0-9]+" :: String) :: (String, String, String))

------------
-- REPORTING
------------
reportBadDowngradePkgs :: [String] -> Aura ()
reportBadDowngradePkgs = badReport reportBadDowngradePkgs_1
