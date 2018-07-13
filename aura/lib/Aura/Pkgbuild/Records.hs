-- |
-- Module    : Aura.Pkgbuild.Records
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle the storing of PKGBUILDs.

module Aura.Pkgbuild.Records
  ( hasPkgbuildStored
  , storePkgbuilds
  ) where

import           Aura.Pkgbuild.Base
import           Aura.Types
import           BasePrelude
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Text as T
import           Shelly (Sh, writefile, test_f, shelly, mkdir_p)

---

-- | Does a given package has a PKGBUILD stored?
-- This is `True` when a package has been built successfully once before.
hasPkgbuildStored :: T.Text -> IO Bool
hasPkgbuildStored = shelly . test_f . pkgbuildPath

-- | Write the PKGBUILDs of some `Buildable`s to disk.
storePkgbuilds :: NonEmptySet Buildable -> IO ()
storePkgbuilds bs = shelly $ do
  mkdir_p pkgbuildCache
  traverse_ (\p -> writePkgbuild (bldNameOf p) (_pkgbuild $ pkgbuildOf p)) bs

writePkgbuild :: T.Text -> T.Text -> Sh ()
writePkgbuild name pkgb = writefile (pkgbuildPath name) pkgb
