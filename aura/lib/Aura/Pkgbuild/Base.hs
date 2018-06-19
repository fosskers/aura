{-# LANGUAGE OverloadedStrings #-}

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

module Aura.Pkgbuild.Base where

import           Aura.Pkgbuild.Editing
import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (FilePath)
import qualified Data.Text as T
import           Shelly

---

pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

pkgbuildPath :: T.Text -> FilePath
pkgbuildPath p = pkgbuildCache </> p <.> "pb"

-- One of my favourite functions in this code base.
pbCustomization :: Settings -> Buildable -> Sh Buildable
pbCustomization ss = foldl (>=>) pure [customizepkg ss, hotEdit ss]

-- | Package a Buildable, running the customization handler first.
--
-- REMINDER: This shouldn't be called concurrently. It could seriously mess
-- up user interaction, and there probably aren't enough packages in the list to
-- make the concurrent scheduling worth it.
packageBuildable :: Settings -> Buildable -> IO Package
packageBuildable ss b = do
  b' <- shelly $ pbCustomization ss b
  pure Package { pkgNameOf        = baseNameOf b'
               , pkgVersionOf     = bldVersionOf b'
               , pkgDepsOf        = bldDepsOf b'
               , pkgInstallTypeOf = Build b' }
