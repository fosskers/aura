{-# LANGUAGE OverloadedStrings #-}

-- Library for handling the storing and diff'ing of PKGBUILDs.

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

module Aura.Pkgbuild.Records
  ( storePkgbuilds
  , hasPkgbuildStored
  , readPkgbuild
  , comparePkgbuilds
  ) where

import           Aura.Diff (unidiff)
import           Aura.Pkgbuild.Base
import           Aura.Types
import           BasePrelude
import qualified Data.Text as T
import           Shelly (Sh, writefile, test_f, shelly, readfile, mkdir_p)
import           System.FilePath ((</>))

---

comparePkgbuilds :: T.Text -> T.Text -> T.Text -> T.Text
comparePkgbuilds name old new =
  T.unlines $ unidiff 3 (T.pack $ "a" </> h) (T.pack $ "b" </> h) (T.lines old) (T.lines new)
  where h = T.unpack name </> "PKGBUILD"

hasPkgbuildStored :: T.Text -> IO Bool
hasPkgbuildStored = shelly . test_f . pkgbuildPath

storePkgbuilds :: [Buildable] -> IO ()
storePkgbuilds = shelly . traverse_ (\p -> writePkgbuild (bldNameOf p) (_pkgbuild $ pkgbuildOf p))

readPkgbuild :: T.Text -> IO T.Text
readPkgbuild = shelly . readfile . pkgbuildPath

writePkgbuild :: T.Text -> T.Text -> Sh ()
writePkgbuild name pkgb = do
  mkdir_p pkgbuildCache
  writefile (pkgbuildPath name) pkgb
