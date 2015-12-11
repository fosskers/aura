-- Module for connecting to the AUR servers,
-- downloading PKGBUILDs and source tarballs, and handling them.

{-

Copyright 2012, 2013, 2014, 2015 Colin Woodbury <colingw@gmail.com>

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

import           Control.Monad        ((>=>), join)
import           Data.Function        (on)
import           Data.List            (sortBy)
import           Data.Monoid          ((<>))
import           Data.Maybe
import qualified Data.Text            as T
import           Linux.Arch.Aur
import           Filesystem.Path.CurrentOS      (FilePath,(</>),fromText)

import           Aura.Monad.Aura
import           Aura.Pkgbuild.Base
import           Aura.Settings.Base
import           Aura.Core

import           Utilities            (decompress)
import           Internet
import           Prelude hiding (FilePath)

---

aurLookup :: T.Text -> Aura (Maybe Buildable)
aurLookup name = fmap (makeBuildable name) <$> pkgbuild' name

aurRepo :: Repository
aurRepo = Repository $ aurLookup >=> traverse packageBuildable

makeBuildable :: T.Text -> Pkgbuild -> Buildable
makeBuildable name pb = Buildable
    { baseNameOf   = name
    , pkgbuildOf   = pb
    , isExplicit   = False
    , buildScripts = f }
    where f :: FilePath -> Aura (Maybe FilePath)
          f fp = liftIO ( sourceTarball fp name ) >>= traverse (liftShelly . decompress)

isAurPackage :: T.Text -> Aura Bool
isAurPackage name = isJust <$> pkgbuild' name

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
sourceTarball :: FilePath            -- ^ Where to save the tarball.
              -> T.Text              -- ^ Package name.
              -> IO (Maybe FilePath) -- ^ Saved tarball location.
sourceTarball path = fmap join . (info >=> traverse f)
    where f = saveUrlContents path . (aurLink <>) . urlPathOf

------------
-- RPC CALLS
------------
sortAurInfo :: SortScheme -> [AurInfo] -> [AurInfo]
sortAurInfo scheme ai = sortBy compare' ai
  where compare' = case scheme of
                     ByVote         -> \x y -> compare (aurVotesOf y) (aurVotesOf x)
                     Alphabetically -> compare `on` aurNameOf

aurSearch :: T.Text -> Aura [AurInfo]
aurSearch regex = asks sortSchemeOf >>= \scheme ->
                  sortAurInfo scheme <$> search regex

aurInfo :: [T.Text] -> Aura [AurInfo]
aurInfo pkgs = sortAurInfo Alphabetically <$> multiinfo pkgs
