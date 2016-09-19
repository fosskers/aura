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

import           Control.Monad ((>=>))
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Text as T
import           Linux.Arch.Aur
import           System.FilePath ((</>))

import           Aura.Core
import           Aura.Monad.Aura
import           Aura.Pkgbuild.Base
import           Aura.Pkgbuild.Fetch
import           Aura.Settings.Base

import           Utilities (decompress)
import           Internet

---

aurLookup :: String -> Aura (Maybe Buildable)
aurLookup name = fmap (makeBuildable name . T.unpack) <$> pkgbuild name

aurRepo :: Repository
aurRepo = Repository $ aurLookup >=> traverse packageBuildable

makeBuildable :: String -> Pkgbuild -> Buildable
makeBuildable name pb = Buildable
    { baseNameOf   = name
    , pkgbuildOf   = pb
    , isExplicit   = False
    , buildScripts = f }
    where f fp = sourceTarball fp (T.pack name) >>= traverse decompress

isAurPackage :: String -> Aura Bool
isAurPackage name = isJust <$> pkgbuild name

----------------
-- AUR PKGBUILDS
----------------
aurLink :: String
aurLink = "https://aur.archlinux.org"

pkgUrl :: String -> String
pkgUrl pkg = aurLink </> "packages" </> pkg

------------------
-- SOURCE TARBALLS
------------------
sourceTarball :: FilePath            -- ^ Where to save the tarball.
              -> T.Text              -- ^ Package name.
              -> IO (Maybe FilePath) -- ^ Saved tarball location.
sourceTarball path pkg = do
  i <- info [pkg]
  case i of
    [] -> pure Nothing
    (i':_) -> case urlPathOf i' of
      Nothing -> pure Nothing
      Just p  -> saveUrlContents path . (aurLink <>) . T.unpack $ p

------------
-- RPC CALLS
------------
sortAurInfo :: SortScheme -> [AurInfo] -> [AurInfo]
sortAurInfo scheme ai = sortBy compare' ai
  where compare' = case scheme of
                     ByVote -> \x y -> compare (aurVotesOf y) (aurVotesOf x)
                     Alphabetically -> compare `on` aurNameOf

aurSearch :: T.Text -> Aura [AurInfo]
aurSearch regex = asks sortSchemeOf >>= \scheme ->
                  sortAurInfo scheme <$> search regex

aurInfo :: [T.Text] -> Aura [AurInfo]
aurInfo pkgs = sortAurInfo Alphabetically <$> info pkgs
