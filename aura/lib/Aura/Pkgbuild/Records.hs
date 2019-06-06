{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Aura.Pkgbuild.Records
-- Copyright : (c) Colin Woodbury, 2012 - 2019
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
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import           Data.Set.NonEmpty (NESet)
import           Lens.Micro ((^.))
import           RIO
import           System.Path (toFilePath)
import           System.Path.IO (createDirectoryIfMissing, doesFileExist)

---

-- | Does a given package has a PKGBUILD stored?
-- This is `True` when a package has been built successfully once before.
hasPkgbuildStored :: PkgName -> IO Bool
hasPkgbuildStored = doesFileExist . pkgbuildPath

-- | Write the PKGBUILDs of some `Buildable`s to disk.
storePkgbuilds :: NESet Buildable -> IO ()
storePkgbuilds bs = do
  createDirectoryIfMissing True pkgbuildCache
  traverse_ (\p -> writePkgbuild (p ^. field @"name") (p ^. field @"pkgbuild")) bs

writePkgbuild :: PkgName -> Pkgbuild -> IO ()
writePkgbuild pn (Pkgbuild pb) = BL.writeFile (toFilePath $ pkgbuildPath pn) pb
