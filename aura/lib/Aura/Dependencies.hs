{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
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
import           Control.Concurrent.STM.TVar
import           Control.Error.Util (note)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Scheduler hiding (traverse_)
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Data.Versions
import           Data.Witherable (wither)
import           Lens.Micro
import           System.IO (hFlush, stdout)
import           UnliftIO.Exception (catchAny, throwString)

---

-- | To signal if one of the STM actions below wrote a new value to the
-- shared Map.
data Wrote = WroteNothing | WroteNew

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Package -> Eff r (NonEmpty (NonEmptySet Package))
resolveDeps repo ps = do
  ss <- asks settings
  tv <- send $ newTVarIO M.empty
  ts <- send $ newTVarIO S.empty
  liftMaybeM (Failure connectionFailure_1) $
    (Just <$> resolveDeps' ss repo tv ts ps) `catchAny` (const $ pure Nothing)
  m  <- send $ readTVarIO tv
  s  <- send $ readTVarIO ts
  unless (length ps == length m) $ send (putStr "\n")
  let de = conflicts ss m s
  unless (null de) . throwError . Failure $ missingPkg_2 de
  either throwError pure $ sortInstall m

resolveDeps'
  :: Settings
  -> Repository
  -> TVar (Map PkgName Package)
  -- ^ A cache of `Package`s that have already
  -- been checked by the `j` function below.
  -> TVar (Set PkgName)
  -- ^ A cache of packages which have already been
  -- confirmed to be installed, or otherwise "satisfied".
  -> NonEmptySet Package
  -- ^ The set of `Package`s to solve deps for.
  -> IO ()
resolveDeps' ss repo tv ts ps =
  withScheduler_ Par' $ \sch -> traverse_ (scheduleWork sch . h sch) ps
  where
    -- | Handles determining whether further work should be done. It won't
    -- continue to `j` if this `Package` has already been analysed.
    h :: Scheduler IO () -> Package -> IO ()
    h sch p = do
      -- We do the existance check and the `M.insert` in the same transaction to
      -- guarantee that no package will be fully checked more than once.
      w <- atomically $ do
        let pn = pname p
        m <- readTVar tv
        case M.lookup pn m of
          Just _  -> pure WroteNothing
          Nothing -> modifyTVar' tv (M.insert pn p) $> WroteNew
      case w of
        WroteNothing -> pure ()
        WroteNew     -> case p of
          -- Pacman will solve further deps for "repo" packages by itself.
          FromRepo _ -> pure ()
          -- We check for conflicts / recursive deps for AUR packages manually.
          FromAUR b  -> readTVarIO tv >>= j sch b

    -- | Check for the existance of dependencies, as well as for any version
    -- conflicts that they might introduce.
    j :: Scheduler IO () -> Buildable -> Map PkgName Package -> IO ()
    j sch b m = do
      s <- readTVarIO ts
      (ds, sd) <- fmap partitionEithers . wither (satisfied m s) $ b ^. field @"deps"
      atomically $ modifyTVar' ts (<> S.fromList sd)
      forM_ (NEL.nonEmpty $ ds ^.. each . field @"name") $ \deps' -> do
        putStr "." *> hFlush stdout
        repoLookup repo ss (NES.fromNonEmpty deps') >>= \case
          Nothing -> throwString "Connection Error"
          Just (_, goods) -> traverse_ (scheduleWork sch . h sch) goods

    satisfied :: Map PkgName Package -> Set PkgName -> Dep -> IO (Maybe (Either Dep PkgName))
    satisfied m s d
      -- This `Dep` has already been checked.
      | M.member dn m || S.member dn s = pure Nothing
      -- Check if a `Dep` is installed, or is otherwise "satisfied" by some
      -- installed package. This invokes a shell call to Pacman, hence the
      -- desire to cache the result.
      | otherwise = Just . bool (Left d) (Right dn) <$> isSatisfied d
      where
        dn :: PkgName
        dn = d ^. field @"name"

conflicts :: Settings -> Map PkgName Package -> Set PkgName -> [DepError]
conflicts ss m s = foldMap f m
  where pm = M.fromList $ foldr (\p acc -> (pprov p ^. field @"provides" . to PkgName, p) : acc) [] m
        f (FromRepo _) = []
        f (FromAUR b)  = flip mapMaybe (b ^. field @"deps") $ \d ->
          let dn = d ^. field @"name"
          in if | S.member dn s -> Nothing
                | otherwise     -> case M.lookup dn m <|> M.lookup dn pm of
                                    Nothing -> Just $ NonExistant dn
                                    Just p  -> realPkgConflicts ss (b ^. field @"name") p d

sortInstall :: Map PkgName Package -> Either Failure (NonEmpty (NonEmptySet Package))
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
leaves :: Ord a => AdjacencyMap a -> Set a
leaves x = S.filter (null . flip postSet x) $ vertexSet x

-- | Split a graph into batches of mutually independent vertices.
-- Probably O(m * n * log(n)) complexity.
batch :: Ord a => AdjacencyMap a -> [Set a]
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
