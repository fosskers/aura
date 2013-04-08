{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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

-- Handles all ABS related functions.

module Aura.Packages.ABS where

import Text.Regex.PCRE  ((=~))
import Control.Monad    (liftM, void)
import System.FilePath

import Aura.Packages.Repository (filterRepoPkgs)
import Aura.Pacman              (pacmanOutput)
import Aura.Monad.Aura
import Aura.Core
import Aura.Bash

import qualified Aura.Shell as A (quietShellCmd)  -- Aura - Has failure checks
import qualified Shell      as S (quietShellCmd)  -- IO   - Doesn't

---

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String String VersionDemand Pkgbuild Namespace

instance Package ABSPkg where
  pkgNameOf (ABSPkg n _ _ _ _) = n
  versionOf (ABSPkg _ _ v _ _) = v

instance Buildable ABSPkg where
  pkgbuildOf  (ABSPkg _ _ _ p _)  = p
  namespaceOf (ABSPkg _ _ _ _ ns) = ns
  source p fp = do
      let loc = absBasePath </> repoOf p </> pkgNameOf p
      S.quietShellCmd "cp" ["-R",loc,fp]
      return $ fp </> pkgNameOf p
  rewrap (ABSPkg n r v p _) ns = ABSPkg n r v p ns
  buildable pkg = do
      repo <- repository name
      absSync repo name
      pkgbuild <- liftIO . readFile . pkgbuildPath repo $ name
      ABSPkg name repo ver pkgbuild `liftM` namespace name pkgbuild
          where (name,ver) = parseNameAndVersionDemand pkg

instance Show ABSPkg where
    show = pkgNameWithVersionDemand

instance Eq ABSPkg where
  a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

repoOf :: ABSPkg -> String
repoOf (ABSPkg _ r _ _ _) = r

-- | File system root for the synchronised ABS tree.
absBasePath :: FilePath
absBasePath = "/var/abs"

pkgbuildPath :: String -> String -> FilePath
pkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

-- | The repository that a package belongs to.
-- For now, only packages in the three official repos are allowed.
repository :: String -> Aura String
repository p = do
  info <- (head . lines) `liftM` pacmanOutput ["-Si",p]
  let (_,_,repo) = info =~ "Repository[ ]+: " :: (String,String,String)
  if not $ repo =~ "(^core|^community|^extra)"
     then failure $ repo </> p ++ " cannot be synced."
     else return repo

absSync :: String -> String -> Aura ()
absSync repo name = void $ A.quietShellCmd "abs" [repo </> name]

filterABSPkgs :: PkgFilter
filterABSPkgs = filterRepoPkgs
