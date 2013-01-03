-- Handles all `-O` operations

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

import System.Exit (ExitCode)

import Aura.General (getOrphans, (|$|))
import Aura.Settings

import Shell (returnSuccess)

---

displayOrphans :: Settings -> [String] -> IO ExitCode
displayOrphans _ []    = getOrphans >>= mapM_ putStrLn >> returnSuccess
displayOrphans ss pkgs = adoptPkg ss pkgs

adoptPkg :: Settings -> [String] -> IO ExitCode
adoptPkg ss pkgs = ss |$| (pacman ss $ ["-D","--asexplicit"] ++ pkgs)
