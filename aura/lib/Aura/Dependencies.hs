{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TupleSections, MultiWayIf #-}
{-# LANGUAGE TypeApplications, DataKinds, DeriveGeneric #-}

-- |
-- Module    : Aura.Dependencies
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Library for handling package dependencies and version conflicts.

module Aura.Dependencies ( resolveDeps ) where

import           Algebra.Graph.AdjacencyMap
import           Aura.Core
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Generics.Product (field, field')
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Versions
import           Data.Witherable (wither)
import           Lens.Micro
import           Lens.Micro.Extras (view)

---

-- | To signal if one of the STM actions below wrote a new value to the
-- shared Map.
data Wrote = WroteNothing | WroteNew

data Status = Waiting | Working deriving (Eq)

data Pool = Pool { threads :: Int
                 , waiters :: TVar Int
                 , tmap    :: TVar (M.Map PkgName Package)
                 , tqueue  :: TQueue Package
                 } deriving (Generic)

workPool :: NonEmptySet Package -> IO Pool
workPool ps = do
  ts <- getNumCapabilities
  tv <- newTVarIO M.empty
  w  <- newTVarIO 0
  tq <- atomically $ newTQueue >>= \q -> traverse_ (writeTQueue q) ps $> q
  pure $ Pool ts w tv tq

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Package -> Eff r (NonEmpty (NonEmptySet Package))
resolveDeps repo ps = do
  ss   <- ask
  pool <- send $ workPool ps
  de   <- send $ resolveDeps' ss pool repo
  m    <- send $ readTVarIO $ tmap pool
  unless (null de) . throwError . Failure $ missingPkg_2 de
  maybe (throwError $ Failure missingPkg_3) pure $ sortInstall m

-- | An empty list signals success.
resolveDeps' :: Settings -> Pool -> Repository -> IO [DepError]
resolveDeps' ss pool repo = fold <$> replicateConcurrently (threads pool) (g Working)
  where
    -- | Handles the fetching of the next `Package` and detection of overall
    -- completion of the task.
    g :: Status -> IO [DepError]
    g s = do
      mp <- atomically . tryReadTQueue $ tqueue pool
      ws <- atomically . readTVar $ waiters pool
      case (mp, s) of
        (Nothing, Waiting) | ws == threads pool -> pure []
                           | otherwise -> threadDelay 500000 *> g Waiting
        (Nothing, Working) -> atomically (modifyTVar' (waiters pool) succ) *> threadDelay 500000 *> g Waiting
        (Just p,  Waiting) -> atomically (modifyTVar' (waiters pool) pred) *> h p
        (Just p,  Working) -> h p

    -- | Handles determining whether further work should be done. It won't
    -- continue to `j` if this `Package` has already been analysed.
    h :: Package -> IO [DepError]
    h p = do
      w <- atomically $ do
        let pn = pname p
        m <- readTVar $ tmap pool
        case M.lookup pn m of
          Just _  -> pure WroteNothing
          Nothing -> modifyTVar' (tmap pool) (M.insert pn p) $> WroteNew
      case w of
        WroteNothing -> g Working
        WroteNew     -> case p of
          FromRepo _ -> g Working
          FromAUR b  -> j b

    -- | Check for the existance of dependencies, as well as for any version conflicts
    -- they might introduce.
    j :: Buildable -> IO [DepError]
    j b = do
      ds <- wither (\d -> bool (Just d) Nothing <$> isSatisfied d) $ b ^. field @"deps"
      case NEL.nonEmpty $ ds ^.. each . field @"name" of
        Nothing    -> g Working
        Just deps' -> do
          (bads, goods) <- repoLookup repo ss $ NES.fromNonEmpty deps'

          let depsMap = M.fromList $ map (view (field @"name") &&& id) ds
              cnfs    = mapMaybe conflicting $ toList goods
              evils   = map NonExistant (toList bads) <> cnfs

              conflicting :: Package -> Maybe DepError
              conflicting p' = either Just (realPkgConflicts ss (b ^. field @"name") p') $ provider p'

              provider :: Package -> Either DepError Dep
              provider p' = maybe
                            (Left $ BrokenProvides (b ^. field @"name") (pprov p') (pname p'))
                            Right $
                            M.lookup (p' ^. to pprov . field' @"provides" . to PkgName) depsMap
                            <|> M.lookup (pname p') depsMap

          atomically $ traverse_ (writeTQueue (tqueue pool)) goods
          (evils <>) <$> g Working

sortInstall :: M.Map PkgName Package -> Maybe (NonEmpty (NonEmptySet Package))
sortInstall m = NEL.nonEmpty . mapMaybe NES.fromSet . batch $ overlay connected singles
  where f (FromRepo _)  = []
        f p@(FromAUR b) = mapMaybe (\d -> fmap (p,) $ (d ^. field @"name") `M.lookup` m) $ b ^. field @"deps" -- TODO handle "provides"?
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

-- | Questions to be answered in conflict checks:
-- 1. Is the package ignored in `pacman.conf`?
-- 2. Is the version requested different from the one provided by
--    the most recent version?
realPkgConflicts :: Settings -> PkgName -> Package -> Dep -> Maybe DepError
realPkgConflicts ss parent pkg dep
    | pn `elem` toIgnore              = Just $ Ignored failMsg1
    | isVersionConflict reqVer curVer = Just $ VerConflict failMsg2
    | otherwise                       = Nothing
    where pn       = pname pkg
          curVer   = pver pkg & release .~ []
          reqVer   = (dep ^. field @"demand") & _VersionDemand . release .~ []
          lang     = langOf ss
          toIgnore = ignoredPkgsOf $ commonConfigOf ss
          failMsg1 = getRealPkgConflicts_2 pn lang
          failMsg2 = getRealPkgConflicts_1 parent pn (prettyV curVer) (T.pack $ show reqVer) lang

-- | Compares a (r)equested version number with a (c)urrent up-to-date one.
-- The `MustBe` case uses regexes. A dependency demanding version 7.4
-- SHOULD match as `okay` against version 7.4, 7.4.0.1, or even 7.4.0.1-2.
isVersionConflict :: VersionDemand -> Versioning -> Bool
isVersionConflict Anything _     = False
isVersionConflict (LessThan r) c = c >= r
isVersionConflict (MoreThan r) c = c <= r
isVersionConflict (MustBe   r) c = c /= r
isVersionConflict (AtLeast  r) c = c < r
