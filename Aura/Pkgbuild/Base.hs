{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

import Aura.Bash
import Aura.Core
import Aura.Monad.Aura
import Aura.Pkgbuild.Editing
import Aura.Settings.Base

---

pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

toFilename :: String -> FilePath
toFilename = (++ ".pb")

pkgbuildPath :: String -> FilePath
pkgbuildPath p = pkgbuildCache ++ toFilename p

trueVersion :: Namespace -> String
trueVersion ns = pkgver ++ "-" ++ pkgrel
    where pkgver = head $ value ns "pkgver"
          pkgrel = head $ value ns "pkgrel"

pbHandler :: Buildable -> Aura Buildable
pbHandler b = ask >>= \ss -> check ss b
  where check ss | mayHotEdit ss      = hotEdit
                 | useCustomizepkg ss = customizepkg
                 | otherwise          = return

-- Package a Buildable, running the customization handler first.
packageBuildable :: Buildable -> Aura Package
packageBuildable b = do
    b' <- pbHandler b
    ns <- namespace (pkgBaseOf b') (pkgbuildOf b')
    return Package
        { pkgNameOf        = pkgBaseOf b'
        , pkgVersionOf     = trueVersion ns
        , pkgDepsOf        = concatMap (map parseDep . value ns)
                           ["depends", "makedepends", "checkdepends"]
        , pkgInstallTypeOf = Build b'
        }
