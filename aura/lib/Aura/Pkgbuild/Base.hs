{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Aura.Pkgbuild.Base where

import           Aura.Pkgbuild.Editing
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import qualified Data.Text as T
import           System.Path (Path, Absolute, FileExt(..), fromAbsoluteFilePath, fromUnrootedFilePath, (</>), (<.>))

---

-- | The default location: \/var\/cache\/aura\/pkgbuilds\/
pkgbuildCache :: Path Absolute
pkgbuildCache = fromAbsoluteFilePath "/var/cache/aura/pkgbuilds/"

-- | The expected path to a stored PKGBUILD, given some package name.
pkgbuildPath :: PkgName -> Path Absolute
pkgbuildPath (PkgName p) = pkgbuildCache </> fromUnrootedFilePath (T.unpack p) <.> FileExt "pb"

-- | Package a Buildable, running the customization handler first.
--
-- REMINDER: This shouldn't be called concurrently. It could seriously mess
-- up user interaction, and there probably aren't enough packages in the list to
-- make the concurrent scheduling worth it.
packageBuildable :: Settings -> Buildable -> IO Package
packageBuildable ss b = FromAUR <$> hotEdit ss b
