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

module Aura.Packages.Virtual 
  ( VirtualPkg(..)
  , providerPkgOf
  , providingPkg ) where

import Text.Regex.PCRE ((=~))
import Data.Maybe      (fromJust, isNothing)
import Data.List       (nub)

import Aura.Conflicts (isVersionConflict)
import Aura.Pacman    (pacmanOutput)
import Aura.Utils     (splitNameAndVer, splitVer)
import Aura.Packages.Repository
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Core

---

-------------------
-- Virtual Packages
-------------------
-- Virtual packages also contain a record of their providing package.
-- Providing packages are assumed to be Pacman (ABS) packages.
-- Are there any instances where this isn't the case?
data VirtualPkg = VirtualPkg String VersionDemand (Maybe RepoPkg)

instance Package VirtualPkg where
    pkgNameOf (VirtualPkg n _ _) = n
    versionOf (VirtualPkg _ v _) = v
    conflict = virtualConflicts
    package pkg = VirtualPkg name ver `fmap` getProvider pkg
        where (name,ver)    = parseNameAndVersionDemand pkg
              getProvider n = do
                 provider <- providingPkg n
                 case provider of
                   Nothing -> return Nothing
                   Just p  -> Just `fmap` package p

instance Show VirtualPkg where
    show = pkgNameWithVersionDemand

instance Eq VirtualPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

providerPkgOf :: VirtualPkg -> Maybe RepoPkg
providerPkgOf (VirtualPkg _ _ p) = p

-- Yields a virtual package's providing package if there is one.
providingPkg :: String -> Aura (Maybe String)
providingPkg virt = do
  candidates <- providingPkg' virt
  let lined = lines candidates
  if length lined /= 1
     then return Nothing
     else return . Just . head $ lined

-- Unsafe version.
-- Only use on virtual packages that have guaranteed providers.
-- Adding "$" to the pkg name (technically a regex) fixes a bug.
providingPkg' :: String -> Aura String
providingPkg' virt = do
  let (name,_) = splitNameAndVer virt
  nub `fmap` pacmanOutput ["-Ssq",name ++ "$"]

-- This can't be generalized as easily.
virtualConflicts :: Settings -> VirtualPkg -> Maybe ErrMsg
virtualConflicts ss pkg
    | isNothing (providerPkgOf pkg) = Just failMsg1
    | isIgnored provider toIgnore   = Just failMsg2
    | isVersionConflict (versionOf pkg) pVer = Just failMsg3
    | otherwise = Nothing
    where name     = pkgNameOf pkg
          ver      = show $ versionOf pkg
          provider = pkgNameOf . fromJust . providerPkgOf $ pkg
          pVer     = providedVerNum pkg
          lang     = langOf ss
          toIgnore = ignoredPkgsOf ss
          failMsg1 = getVirtualConflicts_1 name lang
          failMsg2 = getVirtualConflicts_2 name provider lang
          failMsg3 = getVirtualConflicts_3 name ver provider pVer lang

providedVerNum :: VirtualPkg -> String
providedVerNum pkg = splitVer match
    where match = info =~ ("[ ]" ++ pkgNameOf pkg ++ ">?=[0-9.]+")
          info  = pkgInfoOf . fromJust . providerPkgOf $ pkg
