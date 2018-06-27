{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}
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
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Graph
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Lens.Micro

---

resolveDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> [Package] -> Eff r [Package]
resolveDeps repo ps = do
  ss <- ask
  tv <- send $ newTVarIO M.empty
  de <- send $ resolveDeps' ss tv repo ps
  m  <- send $ readTVarIO tv
  bool (throwError . Failure $ missingPkg_2 de) (pure . sortInstall $ M.elems m) $ null de

-- | An empty list signals success.
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
          (bads, goods) <- repoLookup repo ss . S.fromList $ map depNameOf deps

          let depsMap     = M.fromList $ map (depNameOf &&& id) deps
              (brk, cnfs) = second catMaybes . partitionEithers $ map conflicting goods
              evils       = map NonExistant (toList bads) <> cnfs <> brk

              conflicting :: Package -> Either DepError (Maybe DepError)
              conflicting p' = broken p' >>= Right . realPkgConflicts ss (pkgNameOf p) p'

              broken :: Package -> Either DepError Dep
              broken p' = maybe (Left $ BrokenProvides (pkgNameOf p) (_provides $ pkgProvidesOf p') (pkgNameOf p'))
                          Right $
                          M.lookup (_provides $ pkgProvidesOf p') depsMap <|> M.lookup (pkgNameOf p') depsMap

          bool (pure evils) (resolveDeps' ss tv repo goods) $ null evils

sortInstall :: [Package] -> [Package]
sortInstall ps = reverse . map ((^. _1) . n) . topSort $ g
  where (g, n, _)  = graphFromEdges $ map toEdge ps
        toEdge pkg = (pkg, pkgNameOf pkg, map depNameOf (pkgDepsOf pkg))
