{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

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
import           Aura.IO
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           Data.Versions hiding (Lens')
import           RIO
import           RIO.Lens (each)
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

toInstallL :: Lens' Resolution (Map PkgName Package)
toInstallL f r = (\m -> r { toInstall = m }) <$> f (toInstall r)

satisfiedL :: Lens' Resolution (Set PkgName)
satisfiedL f r = (\s -> r { satisfied = s }) <$> f (satisfied r)

-- | Given some `Package`s, determine its full dependency graph.
-- The graph is collapsed into layers of packages which are not
-- interdependent, and thus can be built and installed as a group.
--
-- Deeper layers of the result list (generally) depend on the previous layers.
resolveDeps :: Repository -> NonEmpty Package -> RIO Env (NonEmpty (NonEmpty Package))
resolveDeps repo ps = do
  ss <- asks settings
  res <- liftIO $ (Just <$> resolveDeps' ss repo ps) `catchAny` const (pure Nothing)
  Resolution m s <- maybe (throwM $ Failure connectFailure_1) pure res
  unless (length ps == length m) $ putText "\n"
  let de = conflicts ss m s
  unless (null de) . throwM . Failure $ missingPkg_2 de
  either throwM pure $ sortInstall m

-- | Solve dependencies for a set of `Package`s assumed to not be
-- installed/satisfied.
resolveDeps' :: Settings -> Repository -> NonEmpty Package -> IO Resolution
resolveDeps' ss repo ps = resolve (Resolution mempty mempty) ps
  where
    -- | Only searches for packages that we haven't checked yet.
    resolve :: Resolution -> NonEmpty Package -> IO Resolution
    resolve r@(Resolution m _) xs = maybe' (pure r) (NEL.nonEmpty goods) $ \goods' -> do
      let m' = M.fromList . map (pname &&& id) $ toList goods'
          r' = r & toInstallL %~ (<> m')
      these (const $ pure r') (satisfy r') (const $ satisfy r') $ dividePkgs goods'
      where
        goods :: [Package]
        goods = NEL.filter (\p -> not $ pname p `M.member` m) xs

    -- | All dependencies from all potential `Buildable`s.
    allDeps :: NonEmpty Buildable -> Set Dep
    allDeps = foldMap1 (S.fromList . (^.. to bDeps . each))

    -- | Deps which are not yet queued for install.
    freshDeps :: Resolution -> Set Dep -> Set Dep
    freshDeps (Resolution m s) = S.filter f
      where
        f :: Dep -> Bool
        f d = let n = dName d in not $ M.member n m || S.member n s

    -- | Consider only "unsatisfied" deps.
    satisfy :: Resolution -> NonEmpty Buildable -> IO Resolution
    satisfy r bs = maybe' (pure r) (nes . freshDeps r $ allDeps bs) $
      areSatisfied >=> these (lookups r) (pure . r') (\uns sat -> lookups (r' sat) uns)
      where
        r' :: Satisfied -> Resolution
        r' (Satisfied sat) = r & satisfiedL %~ (<> f sat)

        -- | Unique names of some dependencies.
        f :: NonEmpty Dep -> Set PkgName
        f = S.fromList . NEL.toList . NEL.map dName

    -- TODO What about if `repoLookup` reports deps that don't exist?
    -- i.e. the left-hand side of the tuple.
    -- | Lookup unsatisfied deps and recurse the entire lookup process.
    lookups :: Resolution -> Unsatisfied -> IO Resolution
    lookups r (Unsatisfied ds) = do
      let names = NEL.map dName ds
      repoLookup repo ss names >>= \case
        Nothing -> throwString "AUR Connection Error"
        Just (_, could) -> case nes could of
          Nothing    -> throwString "Non-existant deps"
          Just goods -> resolve r goods

conflicts :: Settings -> Map PkgName Package -> Set PkgName -> [DepError]
conflicts ss m s = foldMap f m
  where
    pm :: Map PkgName Package
    pm = M.fromList $ map (\p -> (provides $ pprov p, p)) $ toList m

    f :: Package -> [DepError]
    f (FromRepo _) = []
    f (FromAUR b)  = flip mapMaybe (bDeps b) $ \d ->
      let dn = dName d
      -- Don't do conflict checks for deps which are known to be satisfied on
      -- the system.
      in if S.member dn s then Nothing
         else case M.lookup dn m <|> M.lookup dn pm of
                Nothing -> Just . NonExistant dn $ bName b
                Just p  -> realPkgConflicts ss (bName b) p d

sortInstall :: Map PkgName Package -> Either Failure (NonEmpty (NonEmpty Package))
sortInstall m = case cycles depGraph of
  [] -> note (Failure missingPkg_3) . NEL.nonEmpty . mapMaybe nes $ batch depGraph
  cs -> Left . Failure . missingPkg_4 $ map (NEL.map pname . NAM.vertexList1) cs
  where
    f :: Package -> [(Package, Package)]
    f (FromRepo _)  = []
    f p@(FromAUR b) = mapMaybe (\d -> fmap (p,) $ dName d `M.lookup` m)
      $ bDeps b -- TODO handle "provides"?

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
          reqVer   = dDemand dep & _VersionDemand . release .~ []
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
