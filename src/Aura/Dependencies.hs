{-# LANGUAGE FlexibleContexts #-}
-- Library for handling package dependencies and version conflicts.

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

module Aura.Dependencies ( resolveDeps ) where

import           Control.Monad.State
import           Data.Graph
import           Data.Maybe
import qualified Data.Map as Map

import           Aura.Core
import           Aura.Conflicts
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Settings.Base

import           Utilities           (whenM, tripleFst)

---

resolveDeps :: Repository -> [Package] -> Aura [Package]
resolveDeps repo ps =
    sortInstall . Map.elems <$> execStateT (mapM_ addPkg ps) Map.empty
  where
    addPkg pkg = whenM (isNothing <$> getPkg (pkgNameOf pkg)) $ do
        mapM_ addDep (pkgDepsOf pkg)
        modify $ Map.insert (pkgNameOf pkg) pkg

    addDep dep = do
        mpkg <- getPkg $ depNameOf dep
        case mpkg of
            Nothing  -> findPkg dep
            Just pkg -> lift $ checkConflicts pkg dep

    findPkg dep = whenM (not <$> lift (isSatisfied dep)) $ do
        mpkg <- lift $ repoLookup repo name
        case mpkg of
            Nothing  -> lift $ missingPkg name
            Just pkg -> lift (checkConflicts pkg dep) >> addPkg pkg
      where name = depNameOf dep

    getPkg p = gets $ Map.lookup p

missingPkg :: String -> Aura a
missingPkg name = asks langOf >>= failure . missingPkg_1 name

sortInstall :: [Package] -> [Package]
sortInstall ps = reverse . map (tripleFst . n) . topSort $ g
  where (g,n,_)    = graphFromEdges $ map toEdge ps
        toEdge pkg = (pkg, pkgNameOf pkg, map depNameOf $ pkgDepsOf pkg)
