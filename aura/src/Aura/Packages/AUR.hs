-- | Module for connecting to the AUR servers,
-- downloading PKGBUILDs and source tarballs, and handling them.

{-

Copyright 2012, 2013, 2014, 2015, 2016 Colin Woodbury <colingw@gmail.com>

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

import           BasicPrelude hiding (FilePath, liftIO, (</>))

import qualified Data.Text as T
import           Filesystem.Path.CurrentOS (FilePath, (</>), fromText)
import           Linux.Arch.Aur
import           Network.HTTP.Client (Manager)

import           Aura.Core
import           Aura.Monad.Aura
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Fetch
import           Aura.Settings.Base

import           Internet
import           Utilities (decompress)

---

aurLookup :: T.Text -> Aura (Maybe Buildable)
aurLookup name = asks managerOf >>= \m -> fmap (makeBuildable m name) <$> pkgbuild' m name

aurRepo :: Repository
aurRepo = Repository $ aurLookup >=> traverse packageBuildable

makeBuildable :: Manager -> T.Text -> Pkgbuild -> Buildable
makeBuildable m name pb = Buildable
    { baseNameOf   = name
    , pkgbuildOf   = pb
    , isExplicit   = False
    , buildScripts = f }
    where f :: FilePath -> Aura (Maybe FilePath)
          f fp = sourceTarball m fp name >>= traverse (liftShelly . decompress)

isAurPackage :: T.Text -> Aura Bool
isAurPackage name = asks managerOf >>= \m -> isJust <$> pkgbuild' m name

----------------
-- AUR PKGBUILDS
----------------
aurLink :: T.Text
aurLink = "https://aur.archlinux.org"

pkgUrl :: T.Text -> FilePath
pkgUrl pkg = fromText aurLink </> "packages" </> fromText pkg

------------------
-- SOURCE TARBALLS
------------------
sourceTarball :: Manager             -- ^ The request connection Manager.
              -> FilePath            -- ^ Where to save the tarball.
              -> T.Text              -- ^ Package name.
              -> Aura (Maybe FilePath) -- ^ Saved tarball location.
sourceTarball m path pkg = do
  i <- info m [pkg]
  case i of
    [] -> pure Nothing
    (i':_) -> case urlPathOf i' of
      Nothing -> pure Nothing
      Just p  -> liftIO . saveUrlContents m path . (aurLink <>) $ p

------------
-- RPC CALLS
------------
sortAurInfo :: SortScheme -> [AurInfo] -> [AurInfo]
sortAurInfo scheme ai = sortBy compare' ai
  where compare' = case scheme of
                     ByVote -> \x y -> compare (aurVotesOf y) (aurVotesOf x)
                     Alphabetically -> compare `on` aurNameOf

aurSearch :: T.Text -> Aura [AurInfo]
aurSearch regex = ask >>= \s -> do
  sortAurInfo (sortSchemeOf s) <$> search (managerOf s) regex

aurInfo :: [T.Text] -> Aura [AurInfo]
aurInfo pkgs = asks managerOf >>= \m -> sortAurInfo Alphabetically <$> info m pkgs
