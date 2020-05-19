-- |
-- Module    : Aura.Commands.P
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
--
-- Handles @-P@ flags for analysing PKGBUILDs.

module Aura.Commands.P where

import Aura.Core
import Aura.Languages
import Aura.Pkgbuild.Security
import Aura.Security
import Aura.Types
import RIO

---

{-
What functionality am I looking for?

1. Directory-based analysis.
2. Stdin-based analysis, for piping from -Ap.
3. File-based analysis for passing a full-path.

(1) and (3) are basically the same. (1) can be an alias for (3).

-}

findExploits :: Pkgbuild -> RIO Env ()
findExploits pb = case parsedPB pb of
  Nothing -> throwM $ Failure security_11
  Just l  -> case bannedTerms l of
    []  -> pure ()
    bts -> do
      ss <- asks settings
      liftIO $ traverse_ (displayBannedTerms ss) bts
      throwM $ Failure security_12
