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

module Aura.Packages.Repository where

import Text.Regex.PCRE ((=~))
import Data.List     (intercalate)

import Aura.Pacman (pacmanOutput)
import Aura.Conflicts
import Aura.Core

import Utilities (tripleThrd)

---

data RepoPkg = RepoPkg String VersionDemand String

instance Package RepoPkg where
    pkgNameOf (RepoPkg n _ _) = n
    versionOf (RepoPkg _ v _) = v
    conflict = realPkgConflicts (mostRecentVerNum . pkgInfoOf)
    package pkg = RepoPkg name ver `fmap` pacmanOutput ["-Si",name]
        where (name,ver) = parseNameAndVersionDemand pkg

instance Show RepoPkg where
    show = pkgNameWithVersionDemand

instance Eq RepoPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

pkgInfoOf :: RepoPkg -> String
pkgInfoOf (RepoPkg _ _ i) = i

-- | Get only those packages that are accessible by pacman.
filterRepoPkgs :: PkgFilter
filterRepoPkgs pkgs = do
  repoPkgs <- lines `fmap` pacmanOutput ["-Ssq",pkgs']
  return $ filter (`elem` repoPkgs) pkgs
    where pkgs' = "^(" ++ prep pkgs ++ ")$"
          prep  = specs . intercalate "|"
          specs []     = []
          specs (c:cs) | c `elem` "+" = ['[',c,']'] ++ specs cs
                       | otherwise    = c : specs cs

ignoreRepos :: PkgFilter
ignoreRepos _ = return []

-- | Takes `pacman -Si` output as input.
mostRecentVerNum :: String -> String
mostRecentVerNum info = tripleThrd match
    where match     = thirdLine =~ ": " :: (String,String,String)
          thirdLine = allLines !! 2  -- Version num is always the third line.
          allLines  = lines info
