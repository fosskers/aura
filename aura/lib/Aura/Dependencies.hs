{-# LANGUAGE FlexibleContexts, MonoLocalBinds, TupleSections, MultiWayIf #-}
{-# LANGUAGE TypeApplications, DataKinds #-}

-- |
-- Module    : Aura.Dependencies
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Library for handling package dependencies and version conflicts.

module Aura.Dependencies ( resolveDeps ) where

import           System.IO (stdout, hFlush)
import           Algebra.Graph.AdjacencyMap
import           Aura.Concurrency (throttled_)
import           Aura.Core
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           BasePrelude
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
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

---

-- | To signal if one of the STM actions below wrote a new value to the
-- shared Map.
data Wrote = WroteNothing | WroteNew

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Repository -> NonEmptySet Package -> Eff r (NonEmpty (NonEmptySet Package))
resolveDeps repo ps = do
  ss   <- ask
  tv   <- send $ newTVarIO M.empty
  ts   <- send $ newTVarIO S.empty
  send $ resolveDeps' ss repo tv ts ps
  m    <- send $ readTVarIO tv
  s    <- send $ readTVarIO ts
  let de = conflicts ss m s
  unless (null de) . throwError . Failure $ missingPkg_2 de
  maybe (throwError $ Failure missingPkg_3) pure $ sortInstall m

-- | An empty list signals success.
resolveDeps' :: Settings -> Repository -> TVar (M.Map PkgName Package) -> TVar (S.Set PkgName) -> NonEmptySet Package -> IO ()
resolveDeps' ss repo tv ts ps = throttled_ h ps *> putStr "\n"
  where
    -- | Handles determining whether further work should be done. It won't
    -- continue to `j` if this `Package` has already been analysed.
    h :: TQueue Package -> Package -> IO ()
    h tq p = do
      w <- atomically $ do
        let pn = pname p
        m <- readTVar tv
        case M.lookup pn m of
          Just _  -> pure WroteNothing
          Nothing -> modifyTVar' tv (M.insert pn p) $> WroteNew
      case w of
        WroteNothing -> pure ()
        WroteNew     -> case p of
          FromRepo _ -> pure ()
          FromAUR b  -> atomically (readTVar tv) >>= j tq b

    -- | Check for the existance of dependencies, as well as for any version conflicts
    -- they might introduce.
    j :: TQueue Package -> Buildable -> M.Map PkgName Package -> IO ()
    j tq b m = do
      s  <- atomically $ readTVar ts
      (ds, sd) <- fmap partitionEithers . wither (satisfied m s) $ b ^. field @"deps"
      atomically $ modifyTVar' ts (<> S.fromList sd)
      for_ (NEL.nonEmpty $ ds ^.. each . field @"name") $ \deps' -> do
        putStr "." *> hFlush stdout
        (_, goods) <- repoLookup repo ss $ NES.fromNonEmpty deps'
        unless (null goods) . atomically $ traverse_ (writeTQueue tq) goods

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

sortInstall :: M.Map PkgName Package -> Maybe (NonEmpty (NonEmptySet Package))
sortInstall m = topSort depGraph >> NEL.nonEmpty (mapMaybe NES.fromSet $ batch depGraph)
  where f (FromRepo _)  = []
        f p@(FromAUR b) = mapMaybe (\d -> fmap (p,) $ (d ^. field @"name") `M.lookup` m) $ b ^. field @"deps" -- TODO handle "provides"?
        depGraph  = overlay connected singles
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
