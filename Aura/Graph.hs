{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

module Aura.Graph where

import Data.Graph

import Aura.Core

import Utilities (tripleFst)

---

type DepGraph a = (Graph,(Vertex -> (a,Key,[Key])),(Key -> Bool))
type Key = String

-- | Also returns a function to determine if a given Key
-- is present in the graph.
graph :: Package p => [(p,Key,[Key])] -> DepGraph p
graph nodes = (g,n,v')
    where (g,n,v) = graphFromEdges nodes
          v' k    = case v k of
                      Nothing -> False
                      Just _  -> True

-- | Rebuilds the entire Graph, unfortunately.
graphAdd :: Package p => (p,Key,[Key]) -> DepGraph p -> DepGraph p
graphAdd node (g,n,_) = graph $ node : map n (topSort g)

allNodes :: Package p => DepGraph p -> [p]
allNodes (g,n,_) = reverse . map (tripleFst . n) $ topSort g

raw :: Package p => DepGraph p -> [(p,Key,[Key])]
raw (g,n,_) = reverse . map n $ topSort g
