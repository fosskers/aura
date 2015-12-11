{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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

module Aura.Conflicts where

import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.ICU as Re

import Aura.Core
import Aura.Languages
import Aura.Monad.Aura
import Aura.Settings.Base
import Aura.Utils.Numbers (version)

---

-- Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?

checkConflicts :: Package -> Dep -> Aura ()
checkConflicts p d = ask >>= \ss -> traverse_ failure (realPkgConflicts ss p d)

-- | Must be called with a (f)unction that yields the version number
-- of the most up-to-date form of the package.
realPkgConflicts :: Settings -> Package -> Dep -> Maybe Error
realPkgConflicts ss pkg dep
    | isIgnored name toIgnore         = Just failMsg1
    | isVersionConflict reqVer curVer = Just failMsg2
    | otherwise = Nothing
    where name     = pkgNameOf pkg
          curVer   = pkgVersionOf pkg
          reqVer   = depVerDemandOf dep
          lang     = langOf ss
          toIgnore = ignoredPkgsOf ss
          failMsg1 = getRealPkgConflicts_2 name lang
          failMsg2 = getRealPkgConflicts_1 name curVer (T.pack $ show reqVer) lang

-- Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: VersionDemand -> T.Text -> Bool
isVersionConflict Anything _     = False
isVersionConflict (LessThan r) c = version c >= version r
isVersionConflict (MoreThan r) c = version c <= version r
isVersionConflict (MustBe   r) c = isNothing $ Re.find (Re.regex [] ("^" <> r)) c
isVersionConflict (AtLeast  r) c | version c > version r = False
                                 | isVersionConflict (MustBe r) c = True
                                 | otherwise = False
