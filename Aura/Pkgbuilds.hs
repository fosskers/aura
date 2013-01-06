-- Library for handling the storing and diff'ing of PKGBUILDs.

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

module Aura.Pkgbuilds
    ( comparePkgbuilds
    , hasPkgbuildStored
    , storePkgbuilds
    , readPkgbuild
    , writePkgbuild ) where

import System.Directory (doesFileExist)

import Aura.General (AURPkg, pkgNameOf, pkgbuildOf)
import Aura.Monad.Aura

import ColourDiff

---

pkgbuildCache :: String
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

toFilename :: String -> FilePath
toFilename = (++ ".pb")

pkgbuildPath :: String -> FilePath
pkgbuildPath p = pkgbuildCache ++ toFilename p

comparePkgbuilds :: String -> String -> String
comparePkgbuilds old new = diff (lines old) (lines new)

hasPkgbuildStored :: String -> Aura Bool
hasPkgbuildStored = liftIO . doesFileExist . pkgbuildPath 

storePkgbuilds :: [AURPkg] -> Aura ()
storePkgbuilds = mapM_ (\p -> writePkgbuild (pkgNameOf p) (pkgbuildOf p))

readPkgbuild :: String -> Aura String
readPkgbuild = liftIO . readFile . pkgbuildPath

writePkgbuild :: String -> String -> Aura ()
writePkgbuild name p = liftIO $ writeFile (pkgbuildPath name) p
