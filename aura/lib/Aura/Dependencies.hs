{-# LANGUAGE FlexibleContexts #-}

-- Library for handling package dependencies and version conflicts.

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

module Aura.Dependencies ( resolveDeps ) where

import           Aura.Conflicts
import           Aura.Core
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Settings.Base
import           Aura.Types
import           BasePrelude
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Data.Graph
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Utilities (tripleFst)

---

resolveDeps :: Repository -> [Package] -> Aura (Either Failure [Package])
resolveDeps repo ps = do
  ss <- ask
  tv <- liftIO $ newTVarIO M.empty
  de <- liftIO $ resolveDeps' ss tv repo ps
  m  <- liftIO $ readTVarIO tv
  pure . bool (failure $ missingPkg_2 de) (Right . sortInstall $ M.elems m) $ null de

resolveDeps' :: Settings -> TVar (M.Map T.Text Package) -> Repository -> [Package] -> IO [DepError]
resolveDeps' ss tv repo ps = concat <$> mapConcurrently f ps
  where
    -- | `atomically` ensures that only one instance of a `Package` can
    -- ever be written to the TVar.
    f :: Package -> IO [DepError]
    f p = join . atomically $ do
      m <- readTVar tv
      case M.lookup (pkgNameOf p) m of
        Just _  -> pure $ pure []  -- Bail early, we've checked this package already.
        Nothing -> modifyTVar' tv (M.insert (pkgNameOf p) p) $> do
          deps <- fmap catMaybes . mapConcurrently (\d -> bool (Just d) Nothing <$> isSatisfied d) $ pkgDepsOf p
          (bads, goods) <- partitionEithers <$> mapConcurrently (\d -> g d <$> repoLookup repo ss (depNameOf d)) deps
          bool (pure bads) (resolveDeps' ss tv repo goods) $ null bads

    -- | Check for version conflicts.
    g :: Dep -> Maybe Package -> Either DepError Package
    g d Nothing  = Left . NonExistant $ depNameOf d
    g d (Just p) = maybe (Right p) Left $ realPkgConflicts ss p d

sortInstall :: [Package] -> [Package]
sortInstall ps = reverse . fmap (tripleFst . n) . topSort $ g
  where (g, n, _)  = graphFromEdges $ toEdge <$> ps
        toEdge pkg = (pkg, pkgNameOf pkg, depNameOf <$> pkgDepsOf pkg)
