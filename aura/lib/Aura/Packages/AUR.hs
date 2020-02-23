{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
-- |
-- Module    : Aura.Packages.AUR
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Module for connecting to the AUR servers, downloading PKGBUILDs and package
-- sources.

module Aura.Packages.AUR
  ( -- * Batch Querying
    aurLookup
  , aurRepo
    -- * Single Querying
  , aurInfo
  , aurSearch
    -- * Source Retrieval
  , clone
  , pkgUrl
  ) where

import           Aura.Core
import           Aura.Languages
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (fmapEither)
import           Control.Error.Util (hush, note)
import           Control.Monad.Trans.Maybe
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Generics.Product (field)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import           Data.Versions (versioning)
import           Lens.Micro (each, non, (^..))
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)
import           RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           Servant.Client.Core (responseBody)
import           System.Path
import           System.Path.IO (getCurrentDirectory)
import           System.Process.Typed

---

-- | Attempt to retrieve info about a given `Set` of packages from the AUR.
aurLookup :: Manager -> NESet PkgName -> IO (Maybe (Set PkgName, Set Buildable))
aurLookup m names = runMaybeT $ do
  infos <- MaybeT . fmap hush . info m $ foldr (\(PkgName pn) acc -> pn : acc) [] names
  badsgoods <- lift $ traverseConcurrently Par' (buildable m) infos
  let (bads, goods) = partitionEithers badsgoods
      goodNames     = S.fromList $ goods ^.. each . field @"name"
  pure (S.fromList bads <> NES.toSet names S.\\ goodNames, S.fromList goods)

-- | Yield fully realized `Package`s from the AUR.
aurRepo :: IO Repository
aurRepo = do
  tv <- newTVarIO mempty

  -- TODO Use `data-or` here to offer `Or (NESet PkgName) (NESet Package)`?
  -- Yes that sounds like a good idea :)
  let f :: Settings -> NESet PkgName -> IO (Maybe (Set PkgName, Set Package))
      f ss ps = do
        --- Retrieve cached Packages ---
        cache <- readTVarIO tv
        let (uncached, cached) = fmapEither (\p -> note p $ M.lookup p cache) $ toList ps
        --- Lookup uncached Packages ---
        case NEL.nonEmpty uncached of
          Nothing -> pure $ Just (S.empty, S.fromList cached)
          Just uncached' -> runMaybeT $ do
            (bads, goods) <- MaybeT . aurLookup (managerOf ss) $ NES.fromList uncached'
            pkgs <- lift . traverse (packageBuildable ss) $ toList goods
            --- Update Cache ---
            let m = M.fromList $ map (pname &&& id) pkgs
            liftIO . atomically $ modifyTVar' tv (<> m)
            pure (bads, S.fromList $ cached <> pkgs)

  pure $ Repository tv f

buildable :: Manager -> AurInfo -> IO (Either PkgName Buildable)
buildable m ai = do
  let !bse = PkgName $ pkgBaseOf ai
      mver = hush . versioning $ aurVersionOf ai
  mpb <- getPkgbuild m bse  -- Using the package base ensures split packages work correctly.
  case (,) <$> mpb <*> mver of
    Nothing        -> pure . Left . PkgName $ aurNameOf ai
    Just (pb, ver) -> pure $ Right Buildable
      { name     = PkgName $ aurNameOf ai
      , version  = ver
      , base     = bse
      , provides = providesOf ai ^. to listToMaybe . non (aurNameOf ai) . to (Provides . PkgName)
      -- TODO This is a potentially naughty mapMaybe, since deps that fail to
      -- parse will be silently dropped. Unfortunately there isn't much to be
      -- done - `aurLookup` and `aurRepo` which call this function only report
      -- existence errors (i.e. "this package couldn't be found at all").
      , deps       = mapMaybe parseDep $ dependsOf ai ++ makeDepsOf ai
      , pkgbuild   = pb
      , isExplicit = False }

----------------
-- AUR PKGBUILDS
----------------
aurLink :: Path Unrooted
aurLink = fromUnrootedFilePath "https://aur.archlinux.org"

-- | A package's home URL on the AUR.
pkgUrl :: PkgName -> Text
pkgUrl (PkgName pkg) = T.pack . toUnrootedFilePath $ aurLink </> fromUnrootedFilePath "packages" </> fromUnrootedFilePath (T.unpack pkg)

-------------------
-- SOURCES FROM GIT
-------------------
-- | Attempt to clone a package source from the AUR.
clone :: Buildable -> IO (Maybe (Path Absolute))
clone b = do
  ec <- runProcess . setStderr closed . setStdout closed $ proc "git" [ "clone", "--depth", "1", toUnrootedFilePath url ]
  case ec of
    (ExitFailure _) -> pure Nothing
    ExitSuccess     -> do
      pwd <- getCurrentDirectory
      pure . Just $ pwd </> (b ^. field @"base" . field @"name" . to (fromUnrootedFilePath . T.unpack))
  where url = aurLink </> (b ^. field @"base" . field @"name" . to (fromUnrootedFilePath . T.unpack)) <.> FileExt "git"

------------
-- RPC CALLS
------------
sortAurInfo :: Maybe BuildSwitch -> [AurInfo] -> [AurInfo]
sortAurInfo bs ai = L.sortBy compare' ai
  where compare' = case bs of
                     Just SortAlphabetically -> compare `on` aurNameOf
                     _ -> \x y -> compare (aurVotesOf y) (aurVotesOf x)

-- | Frontend to the `aur` library. For @-As@.
aurSearch :: Text -> RIO Env [AurInfo]
aurSearch regex = do
  ss  <- asks settings
  res <- liftMaybeM (Failure connectionFailure_1) . fmap hush . liftIO $ search (managerOf ss) regex
  pure $ sortAurInfo (bool Nothing (Just SortAlphabetically) $ switch ss SortAlphabetically) res

-- | Frontend to the `aur` library. For @-Ai@.
aurInfo :: NonEmpty PkgName -> RIO Env [AurInfo]
aurInfo pkgs = do
  logDebug $ "AUR: Looking up " <> display (length pkgs) <> " packages..."
  m <- asks (managerOf . settings)
  liftIO (info m . map (^. field @"name") $ toList pkgs) >>= \case
    Left (ConnectionError _) -> throwM (Failure connectionFailure_1)
    Left (FailureResponse _ r) -> do
      let !resp = display . decodeUtf8Lenient . toStrictBytes $ responseBody r
      logDebug $ "Failed! Server said: " <> resp
      throwM (Failure miscAURFailure_2)
    Left _ -> throwM (Failure miscAURFailure_1)
    Right res -> pure $ sortAurInfo (Just SortAlphabetically) res
