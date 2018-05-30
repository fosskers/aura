{-# LANGUAGE FlexibleContexts #-}

-- Library for handling package dependencies and version conflicts.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colin@fosskers.ca>

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
import           Aura.Utils (scoldAndFail)
import           BasePrelude
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad.State
import           Data.Graph
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly (whenM)
import           Utilities (tripleFst)

---

resolveDepsOrig :: Repository -> [Package] -> Aura [Package]
resolveDepsOrig repo ps = sortInstall . M.elems <$> execStateT (traverse_ addPkg ps) M.empty
  where
    addPkg :: Package -> StateT (M.Map T.Text Package) Aura ()
    addPkg pkg = whenM (isNothing <$> getPkg (pkgNameOf pkg)) $ do
        traverse_ addDep (pkgDepsOf pkg)
        modify $ M.insert (pkgNameOf pkg) pkg

    addDep :: Dep -> StateT (M.Map T.Text Package) Aura ()
    addDep dep = do
        mpkg <- getPkg $ depNameOf dep
        case mpkg of
            Nothing  -> findPkg dep
            Just pkg -> lift $ checkConflicts pkg dep

    findPkg :: Dep -> StateT (M.Map T.Text Package) Aura ()
    findPkg dep = whenM (not <$> lift (isSatisfied dep)) $ do
      ss   <- ask
      mpkg <- lift $ repoLookup repo ss name
      case mpkg of
        Nothing  -> lift . missingPkg $ T.unpack name
        Just pkg -> lift (checkConflicts pkg dep) *> addPkg pkg
        where name = depNameOf dep

    getPkg :: T.Text -> StateT (M.Map T.Text Package) Aura (Maybe Package)
    getPkg p = gets $ M.lookup p

resolveDeps :: Repository -> [Package] -> Aura [Package]
resolveDeps repo ps = do
  ss  <- ask
  tv  <- liftIO $ newTVarIO M.empty
  etp <- liftIO $ resolveDeps' ss tv repo ps
  m   <- liftIO $ readTVarIO tv
  case etp of
    []   -> pure . sortInstall $ M.elems m
    bads -> scoldAndFail . missingPkg_2 $ map T.unpack bads

resolveDeps' :: Settings -> TVar (M.Map T.Text Package) -> Repository -> [Package] -> IO [T.Text]
resolveDeps' ss tv repo ps = concat <$> mapConcurrently f ps
  where
    -- | `atomically` ensures that only one instance of a `Package` can
    -- ever be written to the TVar.
    f :: Package -> IO [T.Text]
    f p = join . atomically $ do
      m <- readTVar tv
      case M.lookup (pkgNameOf p) m of
        Just _  -> pure $ pure []  -- Bail early, we've checked this package already. Do conflicts here?
        Nothing -> modifyTVar' tv (M.insert (pkgNameOf p) p) $> do
          deps <- fmap catMaybes . mapConcurrently (\d -> bool (Just d) Nothing <$> isSatisfied d) $ pkgDepsOf p
          (bads, goods) <- partitionEithers <$> mapConcurrently (\d -> maybe (Left $ depNameOf d) Right <$> repoLookup repo ss (depNameOf d)) deps
          case bads of
            [] -> resolveDeps' ss tv repo goods
            _  -> pure bads

missingPkg :: String -> Aura a
missingPkg name = asks langOf >>= failure . missingPkg_1 name

sortInstall :: [Package] -> [Package]
sortInstall ps = reverse . fmap (tripleFst . n) . topSort $ g
  where (g, n, _)  = graphFromEdges $ toEdge <$> ps
        toEdge pkg = (pkg, pkgNameOf pkg, depNameOf <$> pkgDepsOf pkg)
