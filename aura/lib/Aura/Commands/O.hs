{-# LANGUAGE OverloadedStrings #-}

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

import           Aura.Core (orphans, sudo)
import           Aura.Monad.Aura
import           Aura.Pacman (pacman)
import           Aura.Types
import           BasePrelude
import qualified Data.Text as T
import qualified Data.Text.IO as T

---

displayOrphans :: [T.Text] -> Aura (Either Failure ())
displayOrphans []   = orphans >>= fmap Right . liftIO . traverse_ T.putStrLn
displayOrphans pkgs = adoptPkg pkgs

adoptPkg :: [T.Text] -> Aura (Either Failure ())
adoptPkg pkgs = fmap join . sudo . pacman $ ["-D", "--asexplicit"] <> pkgs
