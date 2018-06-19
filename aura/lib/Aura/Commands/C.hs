{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

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
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.State
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly hiding (path)
import           Utilities

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  [T.Text] -> [T.Text] -> Eff r ()
downgradePackages _ []         = pure ()
downgradePackages pacOpts pkgs = do
  ss    <- ask
  let cachePath = fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  reals <- send . shelly @IO $ pkgsInCache ss pkgs
  reportBadDowngradePkgs $ pkgs \\ reals
  if | null reals -> pure ()
     | otherwise -> do
         cache   <- send . shelly @IO $ cacheContents cachePath
         choices <- traverse (getDowngradeChoice cache) reals
         rethrow . pacman $ "-U" : pacOpts <> map (toTextIgnore . (cachePath </>)) choices

getDowngradeChoice :: (Member (Reader Settings) r, Member IO r) => Cache -> T.Text -> Eff r T.Text
getDowngradeChoice cache pkg = do
  let choices = getChoicesFromCache cache pkg
  asks langOf >>= send . notify . getDowngradeChoice_1 pkg
  send $ getSelection choices

getChoicesFromCache :: Cache -> T.Text -> [T.Text]
getChoicesFromCache (Cache cache) pkg = sort . mapMaybe f $ M.toList cache
  where f (SimplePkg pn _, pth) = bool Nothing (Just $ _pkgpath pth) $ pkg == pn

searchCache :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r ()
searchCache ps = do
  ss <- ask
  matches <- send . shelly @IO $ cacheMatches ss ps
  send . traverse_ (T.putStrLn . _pkgpath) $ sortPkgs matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => FilePath -> Eff r ()
backupCache dir = do
  exists <- send . shelly @IO $ test_d dir
  if | not exists -> throwError $ Failure backupCache_3
     | otherwise  -> confirmBackup dir >>= backup dir

confirmBackup :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => FilePath -> Eff r Cache
confirmBackup dir = do
  ss    <- ask
  cache <- send . shelly @IO . cacheContents . fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  send . notify $ backupCache_4 (T.unpack $ toTextIgnore dir) (langOf ss)
  send . notify $ backupCache_5 (M.size $ _cache cache) (langOf ss)
  okay  <- send $ optionalPrompt @IO ss backupCache_6
  bool (throwError $ Failure backupCache_7) (pure cache) okay

backup :: (Member (Reader Settings) r, Member IO r) => FilePath -> Cache -> Eff r ()
backup dir (Cache cache) = do
  asks langOf >>= send . notify . backupCache_8
  send $ putStrLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: (Member (Reader Settings) r, Member IO r) => FilePath -> [PackagePath] -> Int -> Eff r ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (PackagePath p : ps) n = do
  cachePath <- asks (fromMaybe defaultPackageCache . cachePathOf . commonConfigOf)
  send $ raiseCursorBy 1
  asks langOf >>= send . warn . copyAndNotify_1 n
  send . shelly @IO $ cp (cachePath </> p) (dir </> p)
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Word -> Eff r ()
cleanCache toSave
  | toSave == 0 = asks langOf >>= send . warn . cleanCache_2 >> rethrow (pacman ["-Scc"])  -- TODO Needed?
  | otherwise   = do
      ss <- ask
      send . warn . cleanCache_3 toSave $ langOf ss
      okay <- send $ optionalPrompt @IO ss cleanCache_4
      bool (throwError $ Failure cleanCache_5) (clean (fromIntegral toSave)) okay

clean :: (Member (Reader Settings) r, Member IO r) => Int -> Eff r ()
clean toSave = do
  ss <- ask
  send . notify . cleanCache_6 $ langOf ss
  let cachePath = fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  (Cache cache) <- send . shelly @IO $ cacheContents cachePath
  let !files    = M.elems cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files \\ fold grouped
      filePaths = (\(PackagePath p) -> cachePath </> p) <$> toRemove
  send . shelly @IO $ traverse_ rm filePaths

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Eff r ()
cleanNotSaved = do
  ss <- ask
  send . notify . cleanNotSaved_1 $ langOf ss
  sfs <- getStateFiles
  states <- send $ traverse readState sfs
  let path = fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- send . shelly @IO $ cacheContents path
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  whenM (send . optionalPrompt @IO ss $ cleanNotSaved_2 $ M.size duds) $
    send $ shelly @IO $ traverse_ rm (map (\(PackagePath p) -> path </> p) $ M.elems duds)

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = _spName <$> simplepkg p

------------
-- REPORTING
------------
reportBadDowngradePkgs :: (Member (Reader Settings) r, Member IO r) => [T.Text] -> Eff r ()
reportBadDowngradePkgs = badReport reportBadDowngradePkgs_1
