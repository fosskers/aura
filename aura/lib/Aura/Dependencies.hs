{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module    : Aura.Dependencies
-- Copyright : (c) Colin Woodbury, 2012 - 2020
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
import           Aura.Utils (maybe', putText)
import           Control.Error.Util (note)
import           Data.Generics.Product (field)
import           Data.Semigroup.Foldable (foldMap1)
import           Data.Set.NonEmpty (pattern IsEmpty, pattern IsNonEmpty, NESet)
import qualified Data.Set.NonEmpty as NES
import           Data.These (these)
import           Data.Versions
import           Lens.Micro
import           RIO
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T

---

-- | The results of dependency resolution.
data Resolution = Resolution
  { toInstall :: Map PkgName Package
  , satisfied :: Set PkgName }
  deriving (Generic)

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: Repository -> NESet Package -> RIO Env (NonEmpty (NESet Package))
resolveDeps repo ps = do
  ss <- asks settings
  res <- liftIO $ (Just <$> resolveDeps' ss repo ps) `catchAny` const (pure Nothing)
  Resolution m s <- maybe (throwM $ Failure connectionFailure_1) pure res
  unless (length ps == length m) $ liftIO (putText "\n")
  let de = conflicts ss m s
  unless (null de) . throwM . Failure $ missingPkg_2 de
  either throwM pure $ sortInstall m

-- | Solve dependencies for a set of `Package`s assumed to not be
-- installed/satisfied.
resolveDeps' :: Settings -> Repository -> NESet Package -> IO Resolution
resolveDeps' ss repo ps = resolve (Resolution mempty mempty) ps
  where
    -- | Only searches for packages that we haven't checked yet.
    resolve :: Resolution -> NESet Package -> IO Resolution
    resolve r@(Resolution m _) xs = maybe' (pure r) (NES.nonEmptySet goods) $ \goods' -> do
      let m' = M.fromList (map (pname &&& id) $ toList goods')
          r' = r & field @"toInstall" %~ (<> m')
      these (const $ pure r') (satisfy r') (const $ satisfy r') $ dividePkgs goods'
      where
        goods :: Set Package
        goods = NES.filter (\p -> not $ pname p `M.member` m) xs

    -- | All dependencies from all potential `Buildable`s.
    allDeps :: NESet Buildable -> Set Dep
    allDeps = foldMap1 (S.fromList . (^.. field @"deps" . each))

    -- | Deps which are not yet queued for install.
    freshDeps :: Resolution -> Set Dep -> Set Dep
    freshDeps (Resolution m s) = S.filter f
      where
        f :: Dep -> Bool
        f d = let n = d ^. field @"name" in not $ M.member n m || S.member n s

    -- | Consider only "unsatisfied" deps.
    satisfy :: Resolution -> NESet Buildable -> IO Resolution
    satisfy r bs = maybe' (pure r) (NES.nonEmptySet . freshDeps r $ allDeps bs) $
      areSatisfied >=> these (lookups r) (pure . r') (\uns sat -> lookups (r' sat) uns)
      where
        r' :: Satisfied -> Resolution
        r' (Satisfied sat) = r & field @"satisfied" %~ (<> f sat)

        f :: NESet Dep -> Set PkgName
        f = S.map (^. field @"name") . NES.toSet

    -- TODO What about if `repoLookup` reports deps that don't exist?
    -- i.e. the left-hand side of the tuple.
    -- | Lookup unsatisfied deps and recurse the entire lookup process.
    lookups :: Resolution -> Unsatisfied -> IO Resolution
    lookups r (Unsatisfied ds) = do
      let names = NES.map (^. field @"name") ds
      repoLookup repo ss names >>= \case
        Nothing -> throwString "AUR Connection Error"
        Just (_, IsEmpty) -> throwString "Non-existant deps"
        Just (_, IsNonEmpty goods) -> resolve r goods

conflicts :: Settings -> Map PkgName Package -> Set PkgName -> [DepError]
conflicts ss m s = foldMap f m
  where
    pm :: Map PkgName Package
    pm = M.fromList $ map (\p -> (pprov p ^. field @"provides", p)) $ toList m

    f :: Package -> [DepError]
    f (FromRepo _) = []
    f (FromAUR b)  = flip mapMaybe (b ^. field @"deps") $ \d ->
      let dn = d ^. field @"name"
      -- Don't do conflict checks for deps which are known to be satisfied on
      -- the system.
      in if | S.member dn s -> Nothing
            | otherwise     -> case M.lookup dn m <|> M.lookup dn pm of
                                Nothing -> Just $ NonExistant dn
                                Just p  -> realPkgConflicts ss (b ^. field @"name") p d

sortInstall :: Map PkgName Package -> Either Failure (NonEmpty (NESet Package))
sortInstall m = case cycles depGraph of
  [] -> note (Failure missingPkg_3) . NEL.nonEmpty . mapMaybe NES.nonEmptySet $ batch depGraph
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
