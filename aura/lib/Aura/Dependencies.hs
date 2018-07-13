{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TupleSections, MultiWayIf #-}

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
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Witherable (wither)

---

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Package -> Eff r (NonEmpty (NonEmptySet Package))
resolveDeps repo ps = do
  ss <- ask
  tv <- send $ newTVarIO M.empty
  de <- send $ resolveDeps' ss tv repo ps
  m  <- send $ readTVarIO tv
  unless (null de) . throwError . Failure $ missingPkg_2 de
  maybe (throwError $ Failure missingPkg_3) pure $ sortInstall m

-- | An empty list signals success.
resolveDeps' :: Settings -> TVar (M.Map T.Text Package) -> Repository -> NonEmptySet Package -> IO [DepError]
resolveDeps' ss tv repo ps = fold <$> mapConcurrently f (toList ps)
  where
    -- | `atomically` ensures that only one instance of a `Package` can
    -- ever be written to the TVar.
    f :: Package -> IO [DepError]
    f p = join . atomically $ do
      m <- readTVar tv
      case M.lookup (_pkgName p) m of
        Just _  -> pure $ pure []  -- Bail early, we've checked this package already.
        Nothing -> modifyTVar' tv (M.insert (_pkgName p) p) $> do
          deps <- wither (\d -> bool (Just d) Nothing <$> isSatisfied d) $ _pkgDeps p
          case NEL.nonEmpty $ map depNameOf deps of
            Nothing    -> pure []
            Just deps' -> do
              (bads, goods) <- repoLookup repo ss $ NES.fromNonEmpty deps'

              let depsMap = M.fromList $ map (depNameOf &&& id) deps
                  cnfs    = mapMaybe conflicting $ toList goods
                  evils   = map NonExistant (toList bads) <> cnfs

                  conflicting :: Package -> Maybe DepError
                  conflicting p' = either Just (realPkgConflicts ss (_pkgName p) p') $ provider p'

                  provider :: Package -> Either DepError Dep
                  provider p' = maybe
                                (Left $ BrokenProvides (_pkgName p) (_provides $ _pkgProvides p') (_pkgName p'))
                                Right $
                                M.lookup (_provides $ _pkgProvides p') depsMap <|> M.lookup (_pkgName p') depsMap

              if | not (null evils) -> pure evils
                 | otherwise        -> maybe (pure []) (resolveDeps' ss tv repo) $ NES.fromSet goods

sortInstall :: M.Map T.Text Package -> Maybe (NonEmpty (NonEmptySet Package))
sortInstall m = NEL.nonEmpty . mapMaybe NES.fromSet . batch $ overlay connected singles
  where f p = mapMaybe (\d -> fmap (p,) $ depNameOf d `M.lookup` m) $ _pkgDeps p  -- TODO handle "provides"?
        elems     = M.elems m
        connected = edges $ foldMap f elems
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
