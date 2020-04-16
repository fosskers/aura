-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Aura.Pkgbuild.Base
  ( -- * Paths
    pkgbuildCache
  , pkgbuildPath
  ) where

import           Aura.Types
import           RIO
import           RIO.FilePath
import qualified RIO.Text as T

---

-- | The default location: \/var\/cache\/aura\/pkgbuilds\/
pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

-- | The expected path to a stored PKGBUILD, given some package name.
pkgbuildPath :: PkgName -> FilePath
pkgbuildPath (PkgName p) = pkgbuildCache </> T.unpack p <.> "pb"
