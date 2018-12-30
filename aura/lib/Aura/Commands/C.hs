{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import           Aura.Colour                (red)
import           Aura.Core
import           Aura.Languages
import           Aura.Pacman                (pacman)
import           Aura.Settings
import           Aura.State
import           Aura.Types
import           Aura.Utils
import           BasePrelude
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Generics.Product      (field)
import           Data.List.NonEmpty         (nonEmpty)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Set.NonEmpty          (NonEmptySet)
import qualified Data.Set.NonEmpty          as NES
import qualified Data.Text                  as T
import           Lens.Micro                 ((^?), _Just)
import           System.Path
import           System.Path.IO             (copyFile, doesDirectoryExist,
                                             removeFile)

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PkgName -> Eff r ()
downgradePackages pkgs = do
  ss    <- ask
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  reals <- send $ pkgsInCache ss pkgs
  traverse_ (report red reportBadDowngradePkgs_1) . nonEmpty . toList $ NES.toSet pkgs S.\\ reals
  unless (null reals) $ do
    cache   <- send $ cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) $ toList reals
    liftEitherM . pacman $ "-U" : asFlag (commonConfigOf ss) <> map (toFilePath . path) choices

-- | For a given package, get a choice from the user about which version of it to
-- downgrade to.
getDowngradeChoice :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Cache -> PkgName -> Eff r PackagePath
getDowngradeChoice cache pkg =
  case nonEmpty $ getChoicesFromCache cache pkg of
    Nothing      -> throwError . Failure $ reportBadDowngradePkgs_2 pkg
    Just choices -> do
      ss <- ask
      send . notify ss . getDowngradeChoice_1 pkg $ langOf ss
      send $ getSelection (T.pack . toFilePath . path) choices

getChoicesFromCache :: Cache -> PkgName -> [PackagePath]
getChoicesFromCache (Cache cache) p = sort . M.elems $ M.filterWithKey (\(SimplePkg pn _) _ -> p == pn) cache

-- | Print all package filenames that match a given `T.Text`.
searchCache :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r ()
searchCache ps = do
  ss <- ask
  matches <- send $ cacheMatches ss ps
  send . traverse_ (putStrLn . toFilePath . path) $ sort matches

-- | The destination folder must already exist for the back-up to begin.
backupCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Path Absolute -> Eff r ()
backupCache dir = do
  exists <- send $ doesDirectoryExist dir
  if | not exists -> throwError $ Failure backupCache_3
     | otherwise  -> confirmBackup dir >>= backup dir

confirmBackup :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Path Absolute -> Eff r Cache
confirmBackup dir = do
  ss    <- ask
  cache <- send . cacheContents . either id id . cachePathOf $ commonConfigOf ss
  send . notify ss $ backupCache_4 (toFilePath dir) (langOf ss)
  send . notify ss $ backupCache_5 (M.size $ _cache cache) (langOf ss)
  okay  <- send $ optionalPrompt ss backupCache_6
  bool (throwError $ Failure backupCache_7) (pure cache) okay

backup :: (Member (Reader Settings) r, Member IO r) => Path Absolute -> Cache -> Eff r ()
backup dir (Cache cache) = do
  ss <- ask
  send . notify ss . backupCache_8 $ langOf ss
  send $ putStrLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (M.elems cache) 1

-- | Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: (Member (Reader Settings) r, Member IO r) => Path Absolute -> [PackagePath] -> Int -> Eff r ()
copyAndNotify _ [] _ = pure ()
copyAndNotify dir (PackagePath p : ps) n = do
  ss <- ask
  send $ raiseCursorBy 1
  send . warn ss . copyAndNotify_1 n $ langOf ss
  send $ copyFile p dir
  copyAndNotify dir ps $ n + 1

-- | Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Word -> Eff r ()
cleanCache toSave
  | toSave == 0 = ask >>= \ss -> send (warn ss . cleanCache_2 $ langOf ss) >> liftEitherM (pacman ["-Scc"])
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
  (Cache cache) <- send $ cacheContents cachePath
  let !files    = M.elems cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files \\ fold grouped
  send $ traverse_ removeFile $ map path toRemove

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: (Member (Reader Settings) r, Member IO r) => Eff r ()
cleanNotSaved = do
  ss <- ask
  send . notify ss . cleanNotSaved_1 $ langOf ss
  sfs <- send getStateFiles
  states <- fmap catMaybes . send $ traverse readState sfs
  let cachePath = either id id . cachePathOf $ commonConfigOf ss
  (Cache cache)  <- send $ cacheContents cachePath
  let duds = M.filterWithKey (\p _ -> any (inState p) states) cache
  prop <- send . optionalPrompt ss $ cleanNotSaved_2 $ M.size duds
  when prop . send . traverse_ removeFile . map path $ M.elems duds

-- | Typically takes the contents of the package cache as an argument.
groupByName :: [PackagePath] -> [[PackagePath]]
groupByName pkgs = groupBy sameBaseName $ sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = simplepkg p ^? _Just . field @"name"
