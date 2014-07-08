-- Library for handling the storing and diff'ing of PKGBUILDs.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Pkgbuild.Records where

import System.Directory (doesFileExist)
import System.FilePath  ((</>))

import Aura.Pkgbuild.Base
import Aura.Monad.Aura
import Aura.Core (Buildable, baseNameOf, pkgbuildOf)
import Aura.Diff (unidiff)

import Utilities (readFileUTF8)

---

comparePkgbuilds :: String -> String -> String -> String
comparePkgbuilds name old new =
    unlines $ unidiff 3 ("a" </> h) ("b" </> h) (lines old) (lines new)
  where
    h = name </> "PKGBUILD"

hasPkgbuildStored :: String -> Aura Bool
hasPkgbuildStored = liftIO . doesFileExist . pkgbuildPath 

storePkgbuilds :: [Buildable] -> Aura ()
storePkgbuilds = mapM_ (\p -> writePkgbuild (baseNameOf p) (pkgbuildOf p))

readPkgbuild :: String -> Aura String
readPkgbuild = liftIO . readFileUTF8 . pkgbuildPath

writePkgbuild :: String -> String -> Aura ()
writePkgbuild name p = liftIO $ writeFile (pkgbuildPath name) p
