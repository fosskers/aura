{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

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
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (quietSh)
import           BasePrelude hiding (head)
import           Control.Compactable (traverseEither)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Data.List.NonEmpty (head)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Versions (versioning)
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)
import           Shelly (Sh, run_)
import           System.FilePath ((</>))

---

-- | Attempt to retrieve info about a given `S.Set` of packages from the AUR.
-- This is a signature expected by `InstallOptions`.
aurLookup :: MonadIO m => Settings -> NonEmptySet T.Text -> m (S.Set T.Text, S.Set Buildable)
aurLookup ss names = do
  (bads, goods) <- info m (toList names) >>= traverseEither (buildable m)
  let goodNames = S.fromList $ map bldNameOf goods
  pure (S.fromList bads <> NES.toSet names S.\\ goodNames, S.fromList goods)
    where m = managerOf ss

-- | Yield fully realized `Package`s from the AUR. This is the other signature
-- expected by `InstallOptions`.
aurRepo :: Repository
aurRepo = Repository $ \ss ps -> do
  (bads, goods) <- aurLookup ss ps
  pkgs <- traverse (packageBuildable ss) $ toList goods
  pure (bads, S.fromList pkgs)

buildable :: MonadIO m => Manager -> AurInfo -> m (Either T.Text Buildable)
buildable m ai = do
  mpb <- pkgbuild m (pkgBaseOf ai)  -- Using the package base ensures split packages work correctly.
  case mpb of
    Nothing -> pure . Left $ aurNameOf ai
    Just pb -> pure $ Right Buildable
      { bldNameOf     = aurNameOf ai
      , pkgbuildOf    = Pkgbuild pb
      , bldBaseNameOf = pkgBaseOf ai
      , bldProvidesOf = list (Provides $ aurNameOf ai) (Provides . head) $ providesOf ai
      , bldDepsOf     = mapMaybe parseDep $ dependsOf ai ++ makeDepsOf ai  -- TODO bad mapMaybe?
      , bldVersionOf  = either (const Nothing) Just . versioning $ aurVersionOf ai
      , isExplicit    = False }

----------------
-- AUR PKGBUILDS
----------------
aurLink :: T.Text
aurLink = "https://aur.archlinux.org"

-- | A package's home URL on the AUR.
pkgUrl :: T.Text -> T.Text
pkgUrl pkg = T.pack $ T.unpack aurLink </> "packages" </> T.unpack pkg

-------------------
-- SOURCES FROM GIT
-------------------
-- | Attempt to clone a package source from the AUR.
clone :: Buildable -> Sh (Maybe FilePath)
clone b = do
  (ec, _) <- quietSh $ run_ "git" ["clone", "--depth", "1", aurLink <> "/" <> bldBaseNameOf b <> ".git"]
  case ec of
    (ExitFailure _) -> pure Nothing
    ExitSuccess     -> pure . Just . T.unpack $ bldBaseNameOf b

------------
-- RPC CALLS
------------
sortAurInfo :: Maybe BuildSwitch -> [AurInfo] -> [AurInfo]
sortAurInfo bs ai = sortBy compare' ai
  where compare' = case bs of
                     Just SortAlphabetically -> compare `on` aurNameOf
                     _ -> \x y -> compare (aurVotesOf y) (aurVotesOf x)

-- | Frontend to the `aur` library. For @-As@.
aurSearch :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r [AurInfo]
aurSearch regex = do
  ss  <- ask
  res <- send $ search @IO (managerOf ss) regex
  pure $ sortAurInfo (bool Nothing (Just SortAlphabetically) $ switch ss SortAlphabetically) res

-- | Frontend to the `aur` library. For @-Ai@.
aurInfo :: (Member (Reader Settings) r, Member IO r) => NonEmpty T.Text -> Eff r [AurInfo]
aurInfo pkgs = asks managerOf >>= \m -> sortAurInfo (Just SortAlphabetically) <$> send (info @IO m $ toList pkgs)
