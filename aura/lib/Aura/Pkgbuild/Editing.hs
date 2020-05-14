-- |
-- Module    : Aura.Pkgbuild.Base
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- For handling the editing of PKGBUILDs.

module Aura.Pkgbuild.Editing ( hotEdit ) where

import Aura.IO
import Aura.Languages
import Aura.Settings
import Aura.Types
import Aura.Utils
import RIO
import System.Process.Typed (proc, runProcess)

---

-- | Write a PKGBUILD to the filesystem temporarily, run some effectful
-- function over it, then read it back in before proceeding with
-- package building.
edit :: (FilePath -> IO a) -> Buildable -> IO Buildable
edit f p = do
  writeFileBinary filename . pkgbuild $ bPkgbuild p
  void $ f filename
  newPB <- readFileBinary filename
  pure (p { bPkgbuild = Pkgbuild newPB})
  where
    filename :: FilePath
    filename = "PKGBUILD"

-- | Allow the user to edit the PKGBUILD.
hotEdit :: Settings -> Buildable -> IO Buildable
hotEdit ss b = do
  ans <- optionalPrompt ss (hotEdit_1 $ bName b)
  bool (pure b) f ans
  where
    f :: IO Buildable
    f = withTempDir $ edit (runProcess . proc (editorOf ss) . (:[])) b
