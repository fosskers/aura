-- |
-- Module    : Aura.Conflicts
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Determines if there are version conflicts during dependency resolution.

module Aura.Conflicts ( realPkgConflicts ) where

import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import qualified Data.Text as T
import           Data.Versions (Versioning, prettyV, release)
import           Lens.Micro

---

-- | Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?
realPkgConflicts :: Settings -> T.Text -> Package -> Dep -> Maybe DepError
realPkgConflicts ss parent pkg dep
    | name `elem` toIgnore            = Just $ Ignored failMsg1
    | isNothing curVer                = Just $ UnparsableVersion name
    | isVersionConflict reqVer curVer = Just $ VerConflict failMsg2
    | otherwise                       = Nothing
    where name     = pkgNameOf pkg
          curVer   = pkgVersionOf pkg   & _Just . release .~ []
          reqVer   = depVerDemandOf dep & _VersionDemand . release .~ []
          lang     = langOf ss
          toIgnore = ignoredPkgsOf $ commonConfigOf ss
          failMsg1 = getRealPkgConflicts_2 name lang
          failMsg2 = getRealPkgConflicts_1 parent name (prettyV $ fromJust curVer) (T.pack $ show reqVer) lang

-- | Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: VersionDemand -> Maybe Versioning -> Bool
isVersionConflict Anything _            = False
isVersionConflict (LessThan r) (Just c) = c >= r
isVersionConflict (MoreThan r) (Just c) = c <= r
isVersionConflict (MustBe   r) (Just c) = c /= r
isVersionConflict (AtLeast  r) (Just c) = c < r
isVersionConflict _ _ = True  -- We expect this branch to never occur, due to the `isNothing` check above.
