{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

-- Handles all `-O` operations

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Commands.O where

import           Aura.Core (orphans, sudo, rethrow)
import           Aura.Pacman (pacman)
import           Aura.Settings (Settings)
import           Aura.Types
import           BasePrelude
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

---

displayOrphans :: IO ()
displayOrphans = orphans >>= traverse_ T.putStrLn

adoptPkg :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => S.Set T.Text -> Eff r ()
adoptPkg pkgs = sudo . rethrow . pacman $ ["-D", "--asexplicit"] <> toList pkgs
