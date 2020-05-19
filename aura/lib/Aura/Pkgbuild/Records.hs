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
  , pkgbuildPath
  ) where

import           Aura.Types
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text as T

---

-- | The default location: \/var\/cache\/aura\/pkgbuilds\/
pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

-- | The expected path to a stored PKGBUILD, given some package name.
pkgbuildPath :: PkgName -> FilePath
pkgbuildPath (PkgName p) = pkgbuildCache </> T.unpack p <.> "pb"

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
