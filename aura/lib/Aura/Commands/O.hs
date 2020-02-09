{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module    : Aura.Commands.O
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-O@ flags - those which involve orphan packages.

module Aura.Commands.O ( displayOrphans, adoptPkg ) where

import Aura.Core (Env(..), liftEitherM, orphans, sudo)
import Aura.Pacman (pacman)
import Aura.Types
import Aura.Utils (putTextLn)
import Control.Effect (Carrier, Member)
import Control.Effect.Error (Error)
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Reader (Reader)
import Data.Generics.Product (field)
import Data.Set.NonEmpty (NESet)
import RIO hiding (Reader)

---

-- | Print the result of @pacman -Qqdt@
displayOrphans :: IO ()
displayOrphans = orphans >>= traverse_ (putTextLn . view (field @"name"))

-- | Identical to @-D --asexplicit@.
adoptPkg :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) => NESet PkgName -> m ()
adoptPkg pkgs = sudo . liftEitherM . sendM . pacman $ ["-D", "--asexplicit"] <> asFlag pkgs
