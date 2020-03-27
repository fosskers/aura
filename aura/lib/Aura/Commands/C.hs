{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Aura.Commands.C
-- Copyright : (c) Colin Woodbury, 2012 - 2020
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
import           Aura.IO
import           Aura.Languages
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.Shell
import           Aura.State
import           Aura.Types
import           Data.Generics.Product (field)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import           Lens.Micro ((^?), _Just)
import           RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Path
import           System.Path.IO (copyFile, doesDirectoryExist, removeFile)

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: NESet PkgName -> RIO Env ()
downgradePackages pkgs = do
  ss    <- asks settings
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  reals <- liftIO $ pkgsInCache ss pkgs
  traverse_ (report red reportBadDowngradePkgs_1) . NEL.nonEmpty . toList $ NES.toSet pkgs S.\\ reals
  unless (null reals) $ do
    cache   <- liftIO $ cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) $ toList reals
    liftIO . pacman $ "-U" : asFlag (commonConfigOf ss) <> map (T.pack . toFilePath . path) choices

-- | For a given package, get a choice from the user about which version of it to
-- downgrade to.
getDowngradeChoice :: Cache -> PkgName -> RIO Env PackagePath
getDowngradeChoice cache pkg =
  case NEL.nonEmpty $ getChoicesFromCache cache pkg of
    Nothing      -> throwM . Failure $ reportBadDowngradePkgs_2 pkg
    Just choices -> do
      ss <- asks settings
      liftIO . notify ss . getDowngradeChoice_1 pkg $ langOf ss
      liftIO $ getSelection (T.pack . toFilePath . path) choices

getChoicesFromCache :: Cache -> PkgName -> [PackagePath]
getChoicesFromCache (Cache cache) p = L.sort . M.elems $ M.filterWithKey (\(SimplePkg pn _) _ -> p == pn) cache

-- | Print all package filenames that match a given `Text`.
searchCache :: Text -> RIO Env ()
searchCache ps = do
  ss <- asks settings
  matches <- liftIO $ cacheMatches ss ps
  liftIO . traverse_ (putTextLn . T.pack . toFilePath . path) $ L.sort matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: Path Absolute -> RIO Env ()
backupCache dir = do
  exists <- liftIO $ doesDirectoryExist dir
  if | not exists -> throwM $ Failure backupCache_3
     | otherwise  -> confirmBackup dir >>= backup dir

confirmBackup :: Path Absolute -> RIO Env Cache
confirmBackup dir = do
  ss    <- asks settings
  cache <- liftIO . cacheContents . either id id . cachePathOf $ commonConfigOf ss
  liftIO . notify ss $ backupCache_4 (toFilePath dir) (langOf ss)
  liftIO . notify ss $ backupCache_5 (M.size $ _cache cache) (langOf ss)
  okay  <- liftIO $ optionalPrompt ss backupCache_6
  bool (throwM $ Failure backupCache_7) (pure cache) okay

backup :: Path Absolute -> Cache -> RIO Env ()
backup dir (Cache cache) = do
  ss <- asks settings
  liftIO . notify ss . backupCache_8 $ langOf ss
  liftIO $ putTextLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: Path Absolute -> [PackagePath] -> Int -> RIO Env ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (PackagePath p : ps) n = do
  ss <- asks settings
  liftIO $ raiseCursorBy 1
  liftIO . warn ss . copyAndNotify_1 n $ langOf ss
  liftIO $ copyFile p dir
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: Word -> RIO Env ()
cleanCache toSave
  | toSave == 0 = do
      ss <- asks settings
      liftIO . warn ss . cleanCache_2 $ langOf ss
      liftIO $ pacman ["-Scc"]
  | otherwise = do
      ss <- asks settings
      liftIO . warn ss . cleanCache_3 toSave $ langOf ss
      okay <- liftIO $ optionalPrompt ss cleanCache_4
      bool (throwM $ Failure cleanCache_5) (clean (fromIntegral toSave)) okay

clean :: Int -> RIO Env ()
clean toSave = do
  ss <- asks settings
  liftIO . notify ss . cleanCache_6 $ langOf ss
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache) <- liftIO $ cacheContents cachePath
  let !files    = M.elems cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files L.\\ fold grouped
  liftIO $ traverse_ (removeFile . path) toRemove

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: RIO Env ()
cleanNotSaved = do
  ss <- asks settings
  liftIO . notify ss . cleanNotSaved_1 $ langOf ss
  sfs <- liftIO getStateFiles
  states <- fmap catMaybes . liftIO $ traverse readState sfs
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- liftIO $ cacheContents cachePath
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  prop <- liftIO . optionalPrompt ss $ cleanNotSaved_2 $ M.size duds
  when prop . liftIO . traverse_ (removeFile . path) $ M.elems duds

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = L.groupBy sameBaseName $ L.sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = simplepkg p ^? _Just . field @"name"
