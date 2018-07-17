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
import           BasePrelude hiding (FilePath)
import           Shelly

---

-- | The default location: \/var\/cache\/aura\/pkgbuilds\/
pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds/"

-- | The expected path to a stored PKGBUILD, given some package name.
pkgbuildPath :: PkgName -> FilePath
pkgbuildPath (PkgName p) = pkgbuildCache </> p <.> "pb"

-- One of my favourite functions in this code base.
-- | Allow the user to customize a PKGBUILD, depending on if they specified
-- @--hotedit@ and/or @--custom@.
pbCustomization :: Settings -> Buildable -> Sh Buildable
pbCustomization ss = foldl (>=>) pure [customizepkg ss, hotEdit ss]

-- | Package a Buildable, running the customization handler first.
--
-- REMINDER: This shouldn't be called concurrently. It could seriously mess
-- up user interaction, and there probably aren't enough packages in the list to
-- make the concurrent scheduling worth it.
packageBuildable :: Settings -> Buildable -> IO Package
packageBuildable ss b = do
  b' <- shelly $ pbCustomization ss b
  pure Package { _pkgName        = bldNameOf b'
               , _pkgVersion     = bldVersionOf b'
               , _pkgBaseName    = bldBaseNameOf b'
               , _pkgProvides    = bldProvidesOf b'
               , _pkgDeps        = bldDepsOf b'
               , _pkgInstallType = Build b' }
