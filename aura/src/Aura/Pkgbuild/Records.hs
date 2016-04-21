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

import BasicPrelude hiding (FilePath, writeFile, liftIO, (</>))

import Shelly hiding (liftIO)
import Data.Foldable    (traverse_)

import Aura.Pkgbuild.Base
import Aura.Monad.Aura
import Aura.Core (Buildable, baseNameOf, pkgbuildOf)
import Aura.Diff (unidiff)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Utilities (readFileUTF8, exists)
import Filesystem

---

comparePkgbuilds :: T.Text -> T.Text -> T.Text -> T.Text
comparePkgbuilds name old new =
  T.unlines $ unidiff 3 a b (T.lines old) (T.lines new)
  where
    a = toTextIgnore (("a" :: FilePath) </> h)
    b = toTextIgnore (("b" :: FilePath) </> h)
    h = fromText name </> ("PKGBUILD" :: FilePath)

hasPkgbuildStored :: T.Text -> Aura Bool
hasPkgbuildStored = liftShelly . exists . pkgbuildPath

storePkgbuilds :: [Buildable] -> Aura ()
storePkgbuilds = traverse_ (\p -> writePkgbuild (baseNameOf p) (pkgbuildOf p))

readPkgbuild :: T.Text -> Aura T.Text
readPkgbuild = liftIO . readFileUTF8 . pkgbuildPath

writePkgbuild :: T.Text -> T.Text -> Aura ()
writePkgbuild name = liftIO . writeFile (pkgbuildPath name) . E.encodeUtf8
