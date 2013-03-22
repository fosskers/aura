{-

Copyright 2012, 2013
Colin Woodbury <colingw@gmail.com>
Nicholas Clarke <nicholas.clarke@sanger.ac.uk>

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

module Aura.ABS where

import           Control.Monad   (liftM)
import           Data.List       (intercalate)
import           Data.Maybe      (fromJust)
import           System.FilePath ((</>))

import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Utils      (scoldAndFail)

import           Bash.Base

-- Stuff --

-- | PKGBUILD file read into a string
type PkgBuild = String

-- | File system root for the synchronised ABS tree
absBasePath :: String
absPath = "/var/abs"

data PkgInfo = PkgInfo {
                       -- | Name of the package (not including repo)
                       nameOf           :: String
                       -- | Repository (core, extras, community)
                       , repository    :: String
                       -- | Latest available version
                       , latestVerOf   :: String
                       -- | Path to the relevant directory in the ABS tree
                       , absPathOf     :: String
                       -- | Package description
                       , descriptionOf :: String
                       } deriving (Eq,Show)

-- | Get info about the named package
getABSPkgInfo :: [String] -> Aura [PkgInfo]
getABSPkgInfo [] = return []
getABSPkgInto items = undefined
