{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

-- |
-- Module    : Aura.Commands.C
-- Copyright : (c) Colin Woodbury, 2012 - 2024
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
  , cleanDir ) where

import           Aura.Build (vcsStore)
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
import           Aura.Utils (nes)
import           Control.Monad.Trans.Maybe
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T

---
-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: NonEmpty PkgName -> RIO Env ()
downgradePackages pkgs = do
  ss <- asks settings
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  reals <- liftIO $ pkgsInCache ss pkgsSet
  traverse_ (report red reportBadDowngradePkgs_1) . nes $ pkgsSet S.\\ reals
  unless (null reals) $ do
    cache   <- liftIO $ cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) $ toList reals
    liftIO . pacman (envOf ss) $ "-U" : asFlag (commonConfigOf ss) <> map (T.pack . ppPath) choices
  where
    pkgsSet :: Set PkgName
    pkgsSet = S.fromList $ NEL.toList pkgs

-- | For a given package, get a choice from the user about which version of it to
-- downgrade to.
getDowngradeChoice :: Cache -> PkgName -> RIO Env PackagePath
getDowngradeChoice cache pkg =
  case NEL.nonEmpty $ getChoicesFromCache cache pkg of
    Nothing      -> throwM . Failure . FailMsg $ reportBadDowngradePkgs_2 pkg
    Just choices -> do
      ss <- asks settings
      notify ss $ getDowngradeChoice_1 pkg
      liftIO $ getSelection (T.pack . ppPath) choices

getChoicesFromCache :: Cache -> PkgName -> [PackagePath]
getChoicesFromCache (Cache cache) p = L.sort . M.elems $ M.filterWithKey (\(SimplePkg pn _) _ -> p == pn) cache

-- | Print all package filenames that match a given `Text`.
searchCache :: Text -> RIO Env ()
searchCache ps = do
  ss <- asks settings
  matches <- liftIO $ cacheMatches ss ps
  traverse_ (putTextLn . T.pack . ppPath) $ L.sort matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: FilePath -> RIO Env ()
backupCache dir = do
  exists <- liftIO $ doesDirectoryExist dir
  if not exists
    then throwM . Failure $ FailMsg backupCache_3
    else confirmBackup dir >>= backup dir

confirmBackup :: FilePath -> RIO Env Cache
confirmBackup dir = do
  ss    <- asks settings
  cache <- liftIO . cacheContents . either id id . cachePathOf $ commonConfigOf ss
  notify ss $ backupCache_4 dir
  notify ss $ backupCache_5 (M.size $ _cache cache)
  withOkay ss backupCache_6 backupCache_7 $ pure cache

backup :: FilePath -> Cache -> RIO Env ()
backup dir (Cache cache) = do
  ss <- asks settings
  notify ss backupCache_8
  putTextLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: FilePath -> [PackagePath] -> Int -> RIO Env ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (p : ps) n = do
  ss <- asks settings
  liftIO $ raiseCursorBy 1
  warn ss $ copyAndNotify_1 n
  liftIO $ copyFile (ppPath p) dir
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: Word -> CleanMode -> RIO Env ()
cleanCache toSave mode
  | toSave == 0 = do
      ss <- asks settings
      warn ss cleanCache_2
      liftIO $ pacman (envOf ss) ["-Scc"]
  | otherwise = do
      ss <- asks settings
      let cachePath = either id id . cachePathOf $ commonConfigOf ss
      -- Measuring the cache size before removal --
      beforeCache@(Cache c) <- liftIO $ cacheContents cachePath
      beforeBytes <- liftIO $ cacheSize beforeCache
      notify ss $ cleanCache_7 (fromIntegral $ M.size c) beforeBytes
      -- Proceed with user confirmation --
      let msg = bool cleanCache_9 cleanCache_3 $ mode == Quantity
      warn ss $ msg toSave
      withOkay ss cleanCache_4 cleanCache_5 $ do
        clean toSave mode beforeCache
        afterCache <- liftIO $ cacheContents cachePath
        afterBytes <- liftIO $ cacheSize afterCache
        notify ss $ cleanCache_8 (beforeBytes - afterBytes)

-- | How big, in megabytes, are all the files in the cache?
cacheSize :: Cache -> IO Word
cacheSize (Cache cache) = do
  bytes <- foldl' (+) 0 <$> traverse (getFileSize . ppPath) (M.elems cache)
  pure . floor @Double $ fromIntegral bytes / 1_048_576  -- 1024 * 1024

clean :: Word -> CleanMode -> Cache -> RIO Env ()
clean toSave mode (Cache cache) = do
  ss <- asks settings
  keep <- toKeep
  notify ss cleanCache_6
  liftIO . traverse_ (removeFile . ppPath) $ toRemove keep
  where
    files :: [PackagePath]
    files = M.elems cache

    toKeep :: RIO Env [PackagePath]
    toKeep = case mode of
      Quantity       -> pure $ grouped >>= recent
      AndUninstalled -> do
        env <- asks (envOf . settings)
        fold . catMaybes <$> liftIO (traverseConcurrently Par' (f env) grouped)

    f :: Environment -> [PackagePath] -> IO (Maybe [PackagePath])
    f _ []       = pure Nothing
    f e ps@(a:_) = runMaybeT $ do
      pn <- MaybeT . pure $ simplepkg a
      void . MaybeT . isInstalled e $ spName pn
      pure $ recent ps

    recent :: [PackagePath] -> [PackagePath]
    recent = take (fromIntegral toSave) . reverse

    grouped :: [[PackagePath]]
    grouped = groupByName files

    toRemove :: [PackagePath] -> [PackagePath]
    toRemove keep = files L.\\ keep

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: RIO Env ()
cleanNotSaved = do
  ss <- asks settings
  notify ss cleanNotSaved_1
  sfs <- liftIO getStateFiles
  states <- fmap catMaybes . liftIO $ traverse readState sfs
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- liftIO $ cacheContents cachePath
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  prop <- liftIO . optionalPrompt ss $ cleanNotSaved_2 $ M.size duds
  when prop . liftIO . traverse_ (removeFile . ppPath) $ M.elems duds

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = L.groupBy sameBaseName $ L.sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = spName <$> simplepkg p

-- | Delete all the files in the given path dir
cleanDir :: RIO Env ()
cleanDir = do
  ss <- asks settings
  let !vcsPath = fromMaybe vcsStore . vcsPathOf $ buildConfigOf ss
  exists <- doesDirectoryExist vcsPath
  when exists $ do
    notify ss cleanCache_6
    listDirectory vcsPath >>= traverse_ (removeDirectoryRecursive . (</>) vcsPath)
