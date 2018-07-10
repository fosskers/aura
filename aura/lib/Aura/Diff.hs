{-# LANGUAGE OverloadedStrings #-}

module Aura.Diff where

import Aura.Settings
import BasePrelude hiding (FilePath)
import Shelly

---

-- | Given two filepaths, output the diff of the two files.
-- Output will be coloured unless colour is deactivated by
-- `--color never` or by detection of a non-terminal output
-- target.
--
-- Also yields a copy of the printed text, if there was any.
diff :: Settings -> FilePath -> FilePath -> Sh ()
diff ss f1 f2 = errExit False . run_ "diff" $ c <> ["-u", toTextIgnore f1, toTextIgnore f2]
  where c = bool ["--color"] [] $ shared ss (Colour Never)
