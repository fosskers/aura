{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TupleSections #-}

-- |
-- Module    : Aura.Dependencies
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Library for handling package dependencies and version conflicts.

module Aura.Dependencies ( resolveDeps ) where

import           Algebra.Graph.AdjacencyMap
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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

---

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> [Package] -> Eff r [S.Set Package]
resolveDeps repo ps = do
  ss <- ask
  tv <- send $ newTVarIO M.empty
  de <- send $ resolveDeps' ss tv repo ps
  m  <- send $ readTVarIO tv
  bool (throwError . Failure $ missingPkg_2 de) (pure $ sortInstall m) $ null de

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
          deps <- fmap catMaybes . traverse (\d -> bool (Just d) Nothing <$> isSatisfied d) $ pkgDepsOf p
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

sortInstall :: M.Map T.Text Package -> [S.Set Package]
sortInstall m = batch $ overlay connected singles
  where f p = mapMaybe (\d -> fmap (p,) $ depNameOf d `M.lookup` m) $ pkgDepsOf p  -- TODO handle "provides"?
        elems     = M.elems m
        connected = edges $ concatMap f elems
        singles   = overlays $ map vertex elems

-- | Find the vertices that have no dependencies.
-- O(n) complexity.
leaves :: Ord a => AdjacencyMap a -> S.Set a
leaves x = S.filter (null . flip postSet x) $ vertexSet x

-- | Split a graph into batches of mutually independent vertices.
-- Probably O(m * n * log(n)) complexity.
batch :: Ord a => AdjacencyMap a -> [S.Set a]
batch g | isEmpty g = []
        | otherwise = ls : batch (induce (`S.notMember` ls) g)
  where ls = leaves g
