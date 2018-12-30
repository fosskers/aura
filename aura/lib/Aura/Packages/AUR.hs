{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

-- |
-- Module    : Aura.Packages.AUR
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Module for connecting to the AUR servers, downloading PKGBUILDs and package sources.

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
import           BasePrelude                   hiding (head)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.Throttled  (throttle)
import           Control.Error.Util            (hush)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe
import           Data.Generics.Product         (field)
import qualified Data.Set                      as S
import           Data.Set.NonEmpty             (NonEmptySet)
import qualified Data.Set.NonEmpty             as NES
import qualified Data.Text                     as T
import           Data.Versions                 (versioning)
import           Lens.Micro
import           Lens.Micro                    (each, to, (^.), (^..))
import           Linux.Arch.Aur
import           Network.HTTP.Client           (Manager)
import           System.Path
import           System.Path.IO                (getCurrentDirectory)
import           System.Process.Typed

---

-- | Attempt to retrieve info about a given `S.Set` of packages from the AUR.
aurLookup :: Manager -> NonEmptySet PkgName -> IO (Maybe (S.Set PkgName, S.Set Buildable))
aurLookup m names = runMaybeT $ MaybeT (info m (foldMap (\(PkgName pn) -> [pn]) names)) >>= \infos -> do
  badsgoods <- lift $ throttle (\_ a -> buildable m a) infos >>= atomically . flushTQueue
  let (bads, goods) = partitionEithers badsgoods
      goodNames     = S.fromList $ goods ^.. each . field @"name"
  pure (S.fromList bads <> NES.toSet names S.\\ goodNames, S.fromList goods)

-- | Yield fully realized `Package`s from the AUR.
aurRepo :: Repository
aurRepo = Repository $ \ss ps -> runMaybeT $ do
  (bads, goods) <- MaybeT $ aurLookup (managerOf ss) ps
  pkgs <- lift . traverse (packageBuildable ss) $ toList goods
  pure (bads, S.fromList pkgs)

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
      , provides = providesOf ai ^. to listToMaybe . non (aurNameOf ai) . to Provides
      -- TODO This is a potentially naughty mapMaybe, since deps that fail to parse
      -- will be silently dropped. Unfortunately there isn't much to be done - `aurLookup`
      -- and `aurRepo` which call this function only report existence errors
      -- (i.e. "this package couldn't be found at all").
      , deps       = mapMaybe parseDep $ dependsOf ai ++ makeDepsOf ai
      , pkgbuild   = pb
      , isExplicit = False }

----------------
-- AUR PKGBUILDS
----------------
aurLink :: Path Unrooted
aurLink = fromUnrootedFilePath "https://aur.archlinux.org"

-- | A package's home URL on the AUR.
pkgUrl :: PkgName -> T.Text
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
sortAurInfo bs ai = sortBy compare' ai
  where compare' = case bs of
                     Just SortAlphabetically -> compare `on` aurNameOf
                     _ -> \x y -> compare (aurVotesOf y) (aurVotesOf x)

-- | Frontend to the `aur` library. For @-As@.
aurSearch :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => T.Text -> Eff r [AurInfo]
aurSearch regex = do
  ss  <- ask
  res <- liftMaybeM (Failure connectionFailure_1) $ search (managerOf ss) regex
  pure $ sortAurInfo (bool Nothing (Just SortAlphabetically) $ switch ss SortAlphabetically) res

-- | Frontend to the `aur` library. For @-Ai@.
aurInfo :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => NonEmpty PkgName -> Eff r [AurInfo]
aurInfo pkgs = do
  m   <- asks managerOf
  res <- liftMaybeM (Failure connectionFailure_1) . info m . map (^. field @"name") $ toList pkgs
  pure $ sortAurInfo (Just SortAlphabetically) res
