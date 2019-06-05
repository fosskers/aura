{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
-- Module    : Aura.Core
-- Copyright : (c) Colin Woodbury, 2012 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Core types and functions which belong nowhere else.

module Aura.Core
  ( -- * Types
    Env(..)
  , Repository(..)
  , liftEither, liftEitherM
  , liftMaybe, liftMaybeM
    -- * User Privileges
  , sudo, trueRoot
    -- * Querying the Package Database
  , foreignPackages, orphans, develPkgs
  , Unsatisfied(..), Satisfied(..)
  , areSatisfied, isInstalled
  , checkDBLock
    -- * Misc. Package Handling
  , removePkgs, partitionPkgs, packageBuildable
    -- * IO
  , notify, warn, scold, report
  ) where

import           Aura.Colour
import           Aura.Languages
import           Aura.Pacman
import           Aura.Pkgbuild.Editing (hotEdit)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding ((<>))
import           Control.Compactable (fmapEither)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Map.Strict (Map)
import           Data.Or (Or(..))
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Lens.Micro ((^.))
import           Lens.Micro.Extras (view)
import           System.Path.IO (doesFileExist)

---

--------
-- TYPES
--------

-- | The complete Aura runtime environment. `Repository` has internal caches
-- instantiated in `IO`, while `Settings` is mostly static and derived from
-- command-line arguments.
data Env = Env { repository :: !Repository, settings :: !Settings }

-- | A `Repository` is a place where packages may be fetched from. Multiple
-- repositories can be combined with the `Semigroup` instance. Checks packages
-- in batches for efficiency.
data Repository = Repository
  { repoCache :: !(TVar (Map PkgName Package))
  , repoLookup :: Settings -> NESet PkgName -> IO (Maybe (Set PkgName, Set Package)) }

-- NOTE The `repoCache` value passed to the combined `Repository` constructor is
-- irrelevant, and only sits there for typechecking purposes. Each `Repository`
-- is expected to leverage its own cache within its `repoLookup` function.
instance Semigroup Repository where
  a <> b = Repository (repoCache a) $ \ss ps -> runMaybeT $ do
    items@(bads, goods) <- MaybeT $ repoLookup a ss ps
    case NES.nonEmptySet bads of
      Nothing    -> pure items
      Just bads' -> second (goods <>) <$> MaybeT (repoLookup b ss bads')

---------------------------------
-- Functions common to `Package`s
---------------------------------
-- | Partition a list of packages into pacman and buildable groups. Yes, this is
-- the correct signature. As far as this function (in isolation) is concerned,
-- there is no way to guarantee that the list of `NESet`s will itself be
-- non-empty.
partitionPkgs :: NonEmpty (NESet Package) -> ([Prebuilt], [NESet Buildable])
partitionPkgs = bimap fold f . unzip . map g . toList
  where g = fmapEither toEither . toList
        f = mapMaybe (fmap NES.fromList . NEL.nonEmpty)
        toEither (FromAUR b)  = Right b
        toEither (FromRepo b) = Left b

-- | Package a Buildable, running the customization handler first.
packageBuildable :: Settings -> Buildable -> IO Package
packageBuildable ss b = FromAUR <$> hotEdit ss b

-----------
-- THE WORK
-----------
-- | Lift a common return type into the `Eff` world. Usually used after a
-- `pacman` call.
liftEither :: Member (Error a) r => Either a b -> Eff r b
liftEither = either throwError pure

-- | Like `liftEither`, but the `Either` can be embedded in something else,
-- usually a `Monad`.
liftEitherM :: (Member (Error a) r, Member m r) => m (Either a b) -> Eff r b
liftEitherM = send >=> liftEither

-- | Like `liftEither`, but for `Maybe`.
liftMaybe :: Member (Error a) r => a -> Maybe b -> Eff r b
liftMaybe a = maybe (throwError a) pure

-- | Like `liftEitherM`, but for `Maybe`.
liftMaybeM :: (Member (Error a) r, Member m r) => a -> m (Maybe b) -> Eff r b
liftMaybeM a m = send m >>= liftMaybe a

-- | Action won't be allowed unless user is root, or using sudo.
sudo :: (Member (Reader Env) r, Member (Error Failure) r) => Eff r a -> Eff r a
sudo action =
  asks (hasRootPriv . envOf . settings) >>= bool (throwError $ Failure mustBeRoot_1) action

-- | Stop the user if they are the true root. Building as root isn't allowed
-- since makepkg v4.2.
trueRoot :: (Member (Reader Env) r, Member (Error Failure) r) => Eff r a -> Eff r a
trueRoot action = asks settings >>= \ss ->
  if not (isTrueRoot $ envOf ss) && buildUserOf (buildConfigOf ss) /= Just (User "root")
    then action else throwError $ Failure trueRoot_3

-- | A list of non-prebuilt packages installed on the system.
-- `-Qm` yields a list of sorted values.
foreignPackages :: IO (Set SimplePkg)
foreignPackages = S.fromList . mapMaybe (simplepkg' . strictText) . BL.lines <$> pacmanOutput ["-Qm"]

-- | Packages marked as a dependency, yet are required by no other package.
orphans :: IO (Set PkgName)
orphans = S.fromList . map (PkgName . strictText) . BL.lines <$> pacmanOutput ["-Qqdt"]

-- | Any package whose name is suffixed by git, hg, svn, darcs, cvs, or bzr.
develPkgs :: IO (Set PkgName)
develPkgs = S.filter isDevelPkg . S.map (^. field @"name") <$> foreignPackages
  where isDevelPkg (PkgName pkg) = any (`T.isSuffixOf` pkg) suffixes
        suffixes = ["-git", "-hg", "-svn", "-darcs", "-cvs", "-bzr"]

-- | Returns what it was given if the package is already installed.
-- Reasoning: Using raw bools can be less expressive.
isInstalled :: PkgName -> IO (Maybe PkgName)
isInstalled pkg = bool Nothing (Just pkg) <$> pacmanSuccess ["-Qq", T.unpack (pkg ^. field @"name")]

-- | An @-Rsu@ call.
removePkgs :: (Member (Reader Env) r, Member (Error Failure) r, Member IO r) => NESet PkgName -> Eff r ()
removePkgs pkgs = do
  pacOpts <- asks (commonConfigOf . settings)
  liftEitherM . pacman $ ["-Rsu"] <> asFlag pkgs <> asFlag pacOpts

-- | Depedencies which are not installed, or otherwise provided by some
-- installed package.
newtype Unsatisfied = Unsatisfied (NESet Dep)

-- | The opposite of `Unsatisfied`.
newtype Satisfied = Satisfied (NESet Dep)

-- | Similar to `isSatisfied`, but dependencies are checked in a batch, since
-- @-T@ can accept multiple inputs.
areSatisfied :: NESet Dep -> IO (Or Unsatisfied Satisfied)
areSatisfied ds = do
  unsats <- S.fromList . mapMaybe parseDep <$> unsat
  pure . bimap Unsatisfied Satisfied $ NES.partition (\d -> S.member d unsats) ds
  where
    unsat :: IO [T.Text]
    unsat = pacmanLines $ "-T" : map (T.unpack . renderedDep) (toList ds)

-- | Block further action until the database is free.
checkDBLock :: Settings -> IO ()
checkDBLock ss = do
  locked <- doesFileExist lockFile
  when locked $ (warn ss . checkDBLock_1 $ langOf ss) *> getLine *> checkDBLock ss

-------
-- MISC  -- Too specific for `Utilities.hs` or `Aura.Utils`
-------

-- | Print some message in green with Aura flair.
notify :: Settings -> Doc AnsiStyle -> IO ()
notify ss = putStrLnA ss . green

-- | Print some message in yellow with Aura flair.
warn :: Settings -> Doc AnsiStyle -> IO ()
warn ss = putStrLnA ss . yellow

-- | Print some message in red with Aura flair.
scold :: Settings -> Doc AnsiStyle -> IO ()
scold ss = putStrLnA ss . red

-- | Report a message with multiple associated items. Usually a list of
-- naughty packages.
report :: (Member (Reader Env) r, Member IO r) =>
  (Doc AnsiStyle -> Doc AnsiStyle) -> (Language -> Doc AnsiStyle) -> NonEmpty PkgName -> Eff r ()
report c msg pkgs = do
  ss <- asks settings
  send . putStrLnA ss . c . msg $ langOf ss
  send . T.putStrLn . dtot . colourCheck ss . vsep . map (cyan . pretty . view (field @"name")) $ toList pkgs
