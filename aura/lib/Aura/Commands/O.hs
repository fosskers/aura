{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

-- |
-- Module    : Aura.Commands.O
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-O@ flags - those which involve orphan packages.

module Aura.Commands.O where

import           Aura.Core (orphans, sudo, rethrow)
import           Aura.Pacman (pacman)
import           Aura.Settings (Settings)
import           Aura.Types
import           BasePrelude
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Text as T
import qualified Data.Text.IO as T

---

-- | Print the result of @pacman -Qqdt@
displayOrphans :: IO ()
displayOrphans = orphans >>= traverse_ T.putStrLn

-- | Identical to @-D --asexplicit@.
adoptPkg :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => NonEmptySet T.Text -> Eff r ()
adoptPkg pkgs = sudo . rethrow . pacman $ ["-D", "--asexplicit"] <> toList pkgs
