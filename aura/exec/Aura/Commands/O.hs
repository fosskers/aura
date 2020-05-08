-- |
-- Module    : Aura.Commands.O
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-O@ flags - those which involve orphan packages.

module Aura.Commands.O ( displayOrphans, adoptPkg ) where

import Aura.Core (Env(..), orphans, sudo)
import Aura.IO (putTextLn)
import Aura.Pacman (pacman)
import Aura.Types
import RIO

---

-- | Print the result of @pacman -Qqdt@
displayOrphans :: IO ()
displayOrphans = orphans >>= traverse_ (putTextLn . pnName)

-- | Identical to @-D --asexplicit@.
adoptPkg :: NonEmpty PkgName -> RIO Env ()
adoptPkg pkgs = sudo . liftIO . pacman $ ["-D", "--asexplicit"] <> asFlag pkgs
