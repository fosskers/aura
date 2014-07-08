-- Handles all `-O` operations

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Commands.O
    ( displayOrphans
    , adoptPkg ) where

import Aura.Core   (orphans, sudo)
import Aura.Pacman (pacman)
import Aura.Monad.Aura

---

displayOrphans :: [String] -> Aura ()
displayOrphans []   = orphans >>= liftIO . mapM_ putStrLn
displayOrphans pkgs = adoptPkg pkgs

adoptPkg :: [String] -> Aura ()
adoptPkg pkgs = sudo (pacman $ ["-D","--asexplicit"] ++ pkgs)
