-- |
-- Module    : Aura.Pkgbuild.Records
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle the storing of PKGBUILDs.

module Aura.Pkgbuild.Records
  ( hasPkgbuildStored
  , storePkgbuilds
  ) where

import Aura.Pkgbuild.Base
import Aura.Types
import RIO
import RIO.Directory

---

-- | Does a given package has a PKGBUILD stored?
-- This is `True` when a package has been built successfully once before.
hasPkgbuildStored :: PkgName -> IO Bool
hasPkgbuildStored = doesFileExist . pkgbuildPath

-- | Write the PKGBUILDs of some `Buildable`s to disk.
storePkgbuilds :: NonEmpty Buildable -> IO ()
storePkgbuilds bs = do
  createDirectoryIfMissing True pkgbuildCache
  traverse_ (\p -> writePkgbuild (bName p) (bPkgbuild p)) bs

writePkgbuild :: PkgName -> Pkgbuild -> IO ()
writePkgbuild pn (Pkgbuild pb) = writeFileBinary (pkgbuildPath pn) pb
