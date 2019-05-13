{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module    : Aura.Dependencies
-- Copyright : (c) Colin Woodbury, 2012 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Library for handling package dependencies and version conflicts.

module Aura.Dependencies ( resolveDeps ) where

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.AdjacencyMap.Algorithm (scc)
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NAM
import           Algebra.Graph.ToGraph (isAcyclic)
import           Aura.Core
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Throttled (throttleMaybe_)
import           Control.Error.Util (hush, note)
import           Control.Effect (Carrier, Member)
import           Control.Effect.Error (Error, throwError)
import           Control.Effect.Lift (Lift, sendM)
import           Control.Effect.Reader (Reader, ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Versions
import           Data.Witherable (wither)
import           Lens.Micro
import           System.IO (hFlush, stdout)

---

-- | To signal if one of the STM actions below wrote a new value to the
-- shared Map.
data Wrote = WroteNothing | WroteNew

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: ( Carrier sig m
               , Member (Reader Settings) sig
               , Member (Error Failure) sig
               , Member (Lift IO) sig
               )
            => Repository -> NonEmptySet Package -> m (NonEmpty (NonEmptySet Package))
resolveDeps repo ps = do
  ss <- ask
  tv <- sendM $ newTVarIO M.empty
  ts <- sendM $ newTVarIO S.empty
  liftMaybeM (Failure connectionFailure_1) . sendM $ resolveDeps' ss repo tv ts ps
  m  <- sendM $ readTVarIO tv
  s  <- sendM $ readTVarIO ts
  unless (length ps == length m) $ sendM (putStr "\n")
  let de = conflicts ss m s
  unless (null de) . throwError . Failure $ missingPkg_2 de
  either throwError pure $ sortInstall m

-- | An empty list signals success.
resolveDeps' :: Settings -> Repository -> TVar (M.Map PkgName Package) -> TVar (S.Set PkgName) -> NonEmptySet Package -> IO (Maybe ())
resolveDeps' ss repo tv ts ps = hush <$> throttleMaybe_ h ps
  where
    -- | Handles determining whether further work should be done. It won't
    -- continue to `j` if this `Package` has already been analysed.
    h :: TQueue Package -> Package -> IO (Maybe ())
    h tq p = do
      w <- atomically $ do
        let pn = pname p
        m <- readTVar tv
        case M.lookup pn m of
          Just _  -> pure WroteNothing
          Nothing -> modifyTVar' tv (M.insert pn p) $> WroteNew
      case w of
        WroteNothing -> pure $ Just ()
        WroteNew     -> case p of
          FromRepo _ -> pure $ Just ()
          FromAUR b  -> readTVarIO tv >>= j tq b

    -- | Check for the existance of dependencies, as well as for any version conflicts
    -- they might introduce.
    j :: TQueue Package -> Buildable -> M.Map PkgName Package -> IO (Maybe ())
    j tq b m = do
      s <- readTVarIO ts
      (ds, sd) <- fmap partitionEithers . wither (satisfied m s) $ b ^. field @"deps"
      atomically $ modifyTVar' ts (<> S.fromList sd)
      case NEL.nonEmpty $ ds ^.. each . field @"name" of
        Nothing -> pure $ Just ()
        Just deps' -> do
          putStr "." *> hFlush stdout
          runMaybeT $ MaybeT (repoLookup repo ss $ NES.fromNonEmpty deps') >>= \(_, goods) ->
            unless (null goods) (lift . atomically $ traverse_ (writeTQueue tq) goods)

    satisfied :: M.Map PkgName Package -> S.Set PkgName -> Dep -> IO (Maybe (Either Dep PkgName))
    satisfied m s d | M.member dn m || S.member dn s = pure Nothing
                    | otherwise = Just . bool (Left d) (Right dn) <$> isSatisfied d
      where dn = d ^. field @"name"

conflicts :: Settings -> M.Map PkgName Package -> S.Set PkgName -> [DepError]
conflicts ss m s = foldMap f m
  where pm = M.fromList $ foldr (\p acc -> (pprov p ^. field @"provides" . to PkgName, p) : acc) [] m
        f (FromRepo _) = []
        f (FromAUR b)  = flip mapMaybe (b ^. field @"deps") $ \d ->
          let dn = d ^. field @"name"
          in if | S.member dn s -> Nothing
                | otherwise     -> case M.lookup dn m <|> M.lookup dn pm of
                                    Nothing -> Just $ NonExistant dn
                                    Just p  -> realPkgConflicts ss (b ^. field @"name") p d

sortInstall :: M.Map PkgName Package -> Either Failure (NonEmpty (NonEmptySet Package))
sortInstall m = case cycles depGraph of
  [] -> note (Failure missingPkg_3) . NEL.nonEmpty . mapMaybe NES.fromSet $ batch depGraph
  cs -> Left . Failure . missingPkg_4 $ map (NEL.map pname . NAM.vertexList1) cs
  where f (FromRepo _)  = []
        f p@(FromAUR b) = mapMaybe (\d -> fmap (p,) $ (d ^. field @"name") `M.lookup` m) $ b ^. field @"deps" -- TODO handle "provides"?
        depGraph  = overlay connected singles
        elems     = M.elems m
        connected = edges $ foldMap f elems
        singles   = overlays $ map vertex elems

cycles :: Ord a => AdjacencyMap a -> [NAM.AdjacencyMap a]
cycles = filter (not . isAcyclic) . vertexList . scc

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
          toIgnore = ignoresOf ss
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
