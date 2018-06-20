{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

-- | Module for connecting to the AUR servers,
-- downloading PKGBUILDs and source tarballs, and handling them.

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

module Aura.Packages.AUR
  ( aurLookup
  , aurRepo
  , isAurPackage
  , sourceTarball
  , aurInfo
  , aurSearch
  , pkgUrl
  ) where

import           Aura.Core
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Fetch
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import qualified Data.Text as T
import           Data.Versions (versioning)
import           Internet
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)
import           System.FilePath ((</>))
import           Utilities (decompress)

---

aurLookup :: MonadIO m => Settings -> T.Text -> m (Maybe Buildable)
aurLookup ss name = pkgbuild' m name >>= traverse (makeBuildable m name . Pkgbuild)
  where m = managerOf ss

aurRepo :: Repository
aurRepo = Repository $ \ss p -> aurLookup ss p >>= traverse (packageBuildable ss)

makeBuildable :: MonadIO m => Manager -> T.Text -> Pkgbuild -> m Buildable
makeBuildable m name pb = do
  ai <- head <$> info m [name]
  pure Buildable
    { baseNameOf   = name
    , pkgbuildOf   = pb
    , bldDepsOf    = mapMaybe parseDep $ dependsOf ai ++ makeDepsOf ai  -- TODO bad mapMaybe?
    , bldVersionOf = either (const Nothing) Just . versioning $ aurVersionOf ai
    , isExplicit   = False
    , buildScripts = f }
    where f fp = sourceTarball m fp name >>= traverse (fmap T.unpack . decompress (T.pack fp) . T.pack)

isAurPackage :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r Bool
isAurPackage name = asks managerOf >>= \m -> isJust <$> send (pkgbuild' @IO m name)

----------------
-- AUR PKGBUILDS
----------------
aurLink :: T.Text
aurLink = "https://aur.archlinux.org"

pkgUrl :: T.Text -> T.Text
pkgUrl pkg = T.pack $ T.unpack aurLink </> "packages" </> T.unpack pkg

------------------
-- SOURCE TARBALLS
------------------
sourceTarball :: Manager             -- ^ The request connection Manager.
              -> FilePath            -- ^ Where to save the tarball.
              -> T.Text              -- ^ Package name.
              -> IO (Maybe FilePath) -- ^ Saved tarball location.
sourceTarball m path pkg = do
  i <- info m [pkg]
  case i of
    [] -> pure Nothing
    (i':_) -> case urlPathOf i' of
      Nothing -> pure Nothing
      Just p  -> saveUrlContents m path . T.unpack $ aurLink <> p

------------
-- RPC CALLS
------------
sortAurInfo :: Maybe BuildSwitch -> [AurInfo] -> [AurInfo]
sortAurInfo bs ai = sortBy compare' ai
  where compare' = case bs of
                     Just SortAlphabetically -> compare `on` aurNameOf
                     _ -> \x y -> compare (aurVotesOf y) (aurVotesOf x)

aurSearch :: (Member (Reader Settings) r, Member IO r) => T.Text -> Eff r [AurInfo]
aurSearch regex = do
  ss  <- ask
  res <- send $ search @IO (managerOf ss) regex
  pure $ sortAurInfo (bool Nothing (Just SortAlphabetically) $ switch ss SortAlphabetically) res

aurInfo :: (Member (Reader Settings) r, Member IO r) => [T.Text] -> Eff r [AurInfo]
aurInfo pkgs = asks managerOf >>= \m -> sortAurInfo (Just SortAlphabetically) <$> send (info @IO m pkgs)
