{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import           Aura.Languages
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.State
import           Aura.Types
import           Aura.Utils
import           Control.Effect (Carrier, Member)
import           Control.Effect.Error (Error, throwError)
import           Control.Effect.Lift (Lift, sendM)
import           Control.Effect.Reader (Reader, asks)
import           Data.Generics.Product (field)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import           Lens.Micro ((^?), _Just)
import           RIO hiding (Reader, asks)
import           RIO.List (groupBy, sort, (\\))
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Path
import           System.Path.IO (copyFile, doesDirectoryExist, removeFile)

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  NESet PkgName -> m ()
downgradePackages pkgs = do
  ss    <- asks settings
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  reals <- sendM $ pkgsInCache ss pkgs
  traverse_ (report red reportBadDowngradePkgs_1) . nonEmpty . toList $ NES.toSet pkgs S.\\ reals
  unless (null reals) $ do
    cache   <- sendM $ cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) $ toList reals
    liftEitherM . sendM . pacman $ "-U" : asFlag (commonConfigOf ss) <> map (T.pack . toFilePath . path) choices

-- | For a given package, get a choice from the user about which version of it to
-- downgrade to.
getDowngradeChoice :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  Cache -> PkgName -> m PackagePath
getDowngradeChoice cache pkg =
  case nonEmpty $ getChoicesFromCache cache pkg of
    Nothing      -> throwError . Failure $ reportBadDowngradePkgs_2 pkg
    Just choices -> do
      ss <- asks settings
      sendM . notify ss . getDowngradeChoice_1 pkg $ langOf ss
      sendM $ getSelection (T.pack . toFilePath . path) choices

getChoicesFromCache :: Cache -> PkgName -> [PackagePath]
getChoicesFromCache (Cache cache) p = sort . M.elems $ M.filterWithKey (\(SimplePkg pn _) _ -> p == pn) cache

-- | Print all package filenames that match a given `Text`.
searchCache :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => Text -> m ()
searchCache ps = do
  ss <- asks settings
  matches <- sendM $ cacheMatches ss ps
  sendM . traverse_ (putTextLn . T.pack . toFilePath . path) $ sort matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  Path Absolute -> m ()
backupCache dir = do
  exists <- sendM $ doesDirectoryExist dir
  if | not exists -> throwError $ Failure backupCache_3
     | otherwise  -> confirmBackup dir >>= backup dir

confirmBackup :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  Path Absolute -> m Cache
confirmBackup dir = do
  ss    <- asks settings
  cache <- sendM . cacheContents . either id id . cachePathOf $ commonConfigOf ss
  sendM . notify ss $ backupCache_4 (toFilePath dir) (langOf ss)
  sendM . notify ss $ backupCache_5 (M.size $ _cache cache) (langOf ss)
  okay  <- sendM $ optionalPrompt ss backupCache_6
  bool (throwError $ Failure backupCache_7) (pure cache) okay

backup :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) =>
  Path Absolute -> Cache -> m ()
backup dir (Cache cache) = do
  ss <- asks settings
  sendM . notify ss . backupCache_8 $ langOf ss
  sendM $ putTextLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) =>
  Path Absolute -> [PackagePath] -> Int -> m ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (PackagePath p : ps) n = do
  ss <- asks settings
  sendM $ raiseCursorBy 1
  sendM . warn ss . copyAndNotify_1 n $ langOf ss
  sendM $ copyFile p dir
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  Word -> m ()
cleanCache toSave
  | toSave == 0 = asks settings >>= \ss -> sendM (warn ss . cleanCache_2 $ langOf ss) >> (liftEitherM . sendM . pacman $ ["-Scc"])
  | otherwise   = do
      ss <- asks settings
      sendM . warn ss . cleanCache_3 toSave $ langOf ss
      okay <- sendM $ optionalPrompt ss cleanCache_4
      bool (throwError $ Failure cleanCache_5) (clean (fromIntegral toSave)) okay

clean :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => Int -> m ()
clean toSave = do
  ss <- asks settings
  sendM . notify ss . cleanCache_6 $ langOf ss
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache) <- sendM $ cacheContents cachePath
  let !files    = M.elems cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files \\ fold grouped
  sendM $ traverse_ (removeFile . path) toRemove

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => m ()
cleanNotSaved = do
  ss <- asks settings
  sendM . notify ss . cleanNotSaved_1 $ langOf ss
  sfs <- sendM getStateFiles
  states <- fmap catMaybes . sendM $ traverse readState sfs
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- sendM $ cacheContents cachePath
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  prop <- sendM . optionalPrompt ss $ cleanNotSaved_2 $ M.size duds
  when prop . sendM . traverse_ (removeFile . path) $ M.elems duds

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = groupBy sameBaseName $ sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = simplepkg p ^? _Just . field @"name"
