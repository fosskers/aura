{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

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
import           Aura.Utils
import           Control.Monad.Trans.Maybe
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Versions (versioning)
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import           RIO.Lens (each, non)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Process.Typed

---

-- | Attempt to retrieve info about a given `Set` of packages from the AUR.
aurLookup :: Manager -> NonEmpty PkgName -> IO (Maybe (Set PkgName, Set Buildable))
aurLookup m names = runMaybeT $ do
  infos <- MaybeT . fmap hush . info m $ foldr (\(PkgName pn) acc -> pn : acc) [] names
  badsgoods <- lift $ traverseConcurrently Par' (buildable m) infos
  let (bads, goods) = partitionEithers badsgoods
      goodNames     = S.fromList $ goods ^.. each . to bName
  pure (S.fromList bads <> S.fromList (NEL.toList names) S.\\ goodNames, S.fromList goods)

-- | Yield fully realized `Package`s from the AUR.
aurRepo :: IO Repository
aurRepo = do
  tv <- newTVarIO mempty

  -- TODO Use `data-or` here to offer `Or (NESet PkgName) (NESet Package)`?
  -- Yes that sounds like a good idea :)
  let f :: Settings -> NonEmpty PkgName -> IO (Maybe (Set PkgName, Set Package))
      f ss ps = do
        --- Retrieve cached Packages ---
        cache <- readTVarIO tv
        let (uncached, cached) = fmapEither (\p -> note p $ M.lookup p cache) $ toList ps
        --- Lookup uncached Packages ---
        case NEL.nonEmpty uncached of
          Nothing -> pure $ Just (S.empty, S.fromList cached)
          Just uncached' -> runMaybeT $ do
            (bads, goods) <- MaybeT $ aurLookup (managerOf ss) uncached'
            let !pkgs = map FromAUR $ S.toList goods
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
      { bName     = PkgName $ aurNameOf ai
      , bVersion  = ver
      , bBase     = bse
      , bProvides = providesOf ai ^. to listToMaybe . non (aurNameOf ai) . to (Provides . PkgName)
      -- TODO This is a potentially naughty mapMaybe, since deps that fail to
      -- parse will be silently dropped. Unfortunately there isn't much to be
      -- done - `aurLookup` and `aurRepo` which call this function only report
      -- existence errors (i.e. "this package couldn't be found at all").
      , bDeps       = mapMaybe parseDep $ dependsOf ai ++ makeDepsOf ai
      , bPkgbuild   = pb
      , bIsExplicit = False }

----------------
-- AUR PKGBUILDS
----------------
aurLink :: FilePath
aurLink = "https://aur.archlinux.org"

-- | A package's home URL on the AUR.
pkgUrl :: PkgName -> Text
pkgUrl (PkgName pkg) = T.pack $ aurLink </> "packages" </> T.unpack pkg

-------------------
-- SOURCES FROM GIT
-------------------
-- | Attempt to clone a package source from the AUR.
clone :: Buildable -> IO (Maybe FilePath)
clone b = do
  ec <- runProcess . setStderr closed . setStdout closed
    $ proc "git" [ "clone", "--depth", "1", url ]
  case ec of
    ExitFailure _ -> pure Nothing
    ExitSuccess   -> do
      pwd <- getCurrentDirectory
      pure . Just $ pwd </> pathy
  where
    pathy :: FilePath
    pathy = T.unpack . pnName $ bBase b

    url :: FilePath
    url = aurLink </> pathy <.> "git"

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
  res <- liftMaybeM (Failure connectFailure_1) . fmap hush . liftIO $ search (managerOf ss) regex
  pure $ sortAurInfo (bool Nothing (Just SortAlphabetically) $ switch ss SortAlphabetically) res

-- | Frontend to the `aur` library. For @-Ai@.
aurInfo :: NonEmpty PkgName -> RIO Env [AurInfo]
aurInfo pkgs = do
  logDebug $ "AUR: Looking up " <> display (length pkgs) <> " packages..."
  m <- asks (managerOf . settings)
  sortAurInfo (Just SortAlphabetically) . fold
    <$> traverseConcurrently Par' (work m) (groupsOf 50 $ NEL.toList pkgs)
  where
    work :: Manager -> [PkgName] -> RIO Env [AurInfo]
    work m ps = liftIO (info m $ map pnName ps) >>= \case
      Left (NotFound _) -> throwM (Failure connectFailure_1)
      Left BadJSON -> throwM (Failure miscAURFailure_3)
      Left (OtherAurError e) -> do
        let !resp = display $ decodeUtf8Lenient e
        logDebug $ "Failed! Server said: " <> resp
        throwM (Failure miscAURFailure_1)
      Right res -> pure res
