{-

Copyright 2012 - 2017 Colin Woodbury <colingw@gmail.com>

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

import Aura.Core
import Aura.Monad.Aura
import Aura.Pkgbuild.Editing
import BasePrelude

---

pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

toFilename :: String -> FilePath
toFilename = (<> ".pb")

pkgbuildPath :: String -> FilePath
pkgbuildPath p = pkgbuildCache <> toFilename p

-- One of my favourite functions in this code base.
pbCustomization :: Buildable -> Aura Buildable
pbCustomization = foldl (>=>) pure [customizepkg, hotEdit]

-- | Package a Buildable, running the customization handler first.
packageBuildable :: Buildable -> Aura Package
packageBuildable b = do
    b' <- pbCustomization b
    pure Package
        { pkgNameOf        = baseNameOf b'
        , pkgDepsOf        = bldDepsOf b'
        , pkgVersionOf     = bldVersionOf b'
        , pkgInstallTypeOf = Build b' }
