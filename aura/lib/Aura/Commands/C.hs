{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

-- |
-- Module    : Aura.Commands.C
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-C@ flags - those which involve the package cache.

module Aura.Commands.C
  ( downgradePackages
  , searchCache
  , backupCache
  , cleanCache
  , cleanNotSaved
  ) where

import           Aura.Cache
import           Aura.Colour (red)
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly hiding (path)
import           Utilities

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set T.Text -> Eff r ()
downgradePackages pkgs =
  unless (null pkgs) $ do
    ss    <- ask
    let cachePath = either id id . cachePathOf $ commonConfigOf ss
    reals <- send . shelly @IO $ pkgsInCache ss pkgs
    report red reportBadDowngradePkgs_1 . toList $ pkgs S.\\ reals
    unless (null reals) $ do
      cache   <- send . shelly @IO $ cacheContents cachePath
      choices <- traverse (getDowngradeChoice cache) $ toList reals
      rethrow . pacman $ "-U" : asFlag (commonConfigOf ss) <> map (toTextIgnore . (cachePath </>)) choices

getDowngradeChoice :: (Member (Reader Settings) r, Member IO r) => Cache -> T.Text -> Eff r T.Text
getDowngradeChoice cache pkg = do
  let choices = getChoicesFromCache cache pkg
  ss <- ask
  send . notify ss . getDowngradeChoice_1 pkg $ langOf ss
  send $ getSelection choices

getChoicesFromCache :: Cache -> T.Text -> [T.Text]
getChoicesFromCache (Cache cache) pkg = sort . mapMaybe f $ M.toList cache
  where f (SimplePkg pn _, pth) = bool Nothing (Just $ _pkgpath pth) $ pkg == pn

searchCache :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r ()
searchCache ps = do
  ss <- ask
  matches <- send . shelly @IO $ cacheMatches ss ps
  send . traverse_ (T.putStrLn . _pkgpath) $ sort matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => FilePath -> Eff r ()
backupCache dir = do
  exists <- send . shelly @IO $ test_d dir
  if | not exists -> throwError $ Failure backupCache_3
     | otherwise  -> confirmBackup dir >>= backup dir

confirmBackup :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => FilePath -> Eff r Cache
confirmBackup dir = do
  ss    <- ask
  cache <- send . shelly @IO . cacheContents . either id id . cachePathOf $ commonConfigOf ss
  send . notify ss $ backupCache_4 (T.unpack $ toTextIgnore dir) (langOf ss)
  send . notify ss $ backupCache_5 (M.size $ _cache cache) (langOf ss)
  okay  <- send $ optionalPrompt ss backupCache_6
  bool (throwError $ Failure backupCache_7) (pure cache) okay

backup :: (Member (Reader Settings) r, Member IO r) => FilePath -> Cache -> Eff r ()
backup dir (Cache cache) = do
  ss <- ask
  send . notify ss . backupCache_8 $ langOf ss
  send $ putStrLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: (Member (Reader Settings) r, Member IO r) => FilePath -> [PackagePath] -> Int -> Eff r ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (PackagePath p : ps) n = do
  ss <- ask
  cachePath <- asks (either id id . cachePathOf . commonConfigOf)
  send $ raiseCursorBy 1
  send . warn ss . copyAndNotify_1 n $ langOf ss
  send . shelly @IO $ cp (cachePath </> p) (dir </> p)
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Word -> Eff r ()
cleanCache toSave
  | toSave == 0 = ask >>= \ss -> send (warn ss . cleanCache_2 $ langOf ss) >> rethrow (pacman ["-Scc"])
  | otherwise   = do
      ss <- ask
      send . warn ss . cleanCache_3 toSave $ langOf ss
      okay <- send $ optionalPrompt ss cleanCache_4
      bool (throwError $ Failure cleanCache_5) (clean (fromIntegral toSave)) okay

clean :: (Member (Reader Settings) r, Member IO r) => Int -> Eff r ()
clean toSave = do
  ss <- ask
  send . notify ss . cleanCache_6 $ langOf ss
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
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
  send . notify ss . cleanNotSaved_1 $ langOf ss
  sfs <- getStateFiles
  states <- fmap catMaybes . send $ traverse readState sfs
  let path = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- send . shelly @IO $ cacheContents path
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  whenM (send . optionalPrompt ss $ cleanNotSaved_2 $ M.size duds) $
    send $ shelly @IO $ traverse_ rm (map (\(PackagePath p) -> path </> p) $ M.elems duds)

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = groupBy sameBaseName $ sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = _spName <$> simplepkg p
