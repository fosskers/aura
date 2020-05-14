-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- For handling the editing of PKGBUILDs, .install files, and .patch files.

module Aura.Pkgbuild.Editing ( edit ) where

import Aura.Settings
import RIO
import System.Process.Typed (proc, runProcess)

---

-- TODO Move to Utils and delete this module.
-- | Edit some file in-place with the user's specified editor.
edit :: Settings -> FilePath -> IO ()
edit ss p = void . runProcess $ proc (editorOf ss) [p]
