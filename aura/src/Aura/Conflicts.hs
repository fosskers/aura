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

module Aura.Conflicts where

import           Aura.Core
import           Aura.Errors
import           Aura.Languages
import           Aura.Settings.Base
import           BasePrelude
import qualified Data.Text as T
import           Text.Regex.PCRE ((=~))

---

-- TODO Get rid of this
version :: String -> Int
version = undefined

-- Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?

realPkgConflicts :: Settings -> Package -> Dep -> Maybe DepError
realPkgConflicts ss pkg dep
    | name `elem` toIgnore                       = Just $ Ignored failMsg1
    | isVersionConflict reqVer (T.unpack curVer) = Just $ VerConflict failMsg2
    | otherwise                                  = Nothing
    where name     = pkgNameOf pkg
          curVer   = pkgVersionOf pkg
          reqVer   = depVerDemandOf dep
          lang     = langOf ss
          toIgnore = ignoredPkgsOf ss
          failMsg1 = getRealPkgConflicts_2 (T.unpack name) lang
          failMsg2 = getRealPkgConflicts_1 (T.unpack name) (T.unpack curVer) (show reqVer) lang

-- Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: VersionDemand -> String -> Bool
isVersionConflict Anything _     = False
isVersionConflict (LessThan r) c = version c >= version r
isVersionConflict (MoreThan r) c = version c <= version r
isVersionConflict (MustBe   r) c = not $ c =~ ("^" <> r)
isVersionConflict (AtLeast  r) c | version c > version r = False
                                 | isVersionConflict (MustBe r) c = True
                                 | otherwise = False
