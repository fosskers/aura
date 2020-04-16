-- |
-- Module    : Aura.Diff
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Diffing files.

module Aura.Diff ( diff ) where

import Aura.Settings
import RIO
import System.Process.Typed (proc, runProcess)

---

-- | Given two filepaths, output the diff of the two files.
-- Output will be coloured unless colour is deactivated by
-- `--color never` or by detection of a non-terminal output
-- target.
diff :: MonadIO m => Settings -> FilePath -> FilePath -> m ()
diff ss f1 f2 = void . runProcess . proc "diff" $ c <> ["-u", f1, f2]
  where
    c :: [FilePath]
    c = bool ["--color"] [] $ shared ss (Colour Never)
