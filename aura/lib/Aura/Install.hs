{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Aura.Install
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Layer for AUR package installation.
-- Backend for `Aura.Commands.A`.

module Aura.Install
  ( install
  , displayPkgDeps
  ) where

import           Aura.Build
import           Aura.Cache (Cache(..), cacheContents)
import           Aura.Colour
import           Aura.Core
import           Aura.Dependencies (resolveDeps)
import           Aura.IO
import           Aura.Languages
import           Aura.Packages.AUR (aurLookup)
import           Aura.Pacman (pacman, pacmanSuccess)
import           Aura.Pkgbuild.Records
import           Aura.Security
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           RIO
import           RIO.Directory (setCurrentDirectory)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S

---

-- | High level 'install' command. Handles installing dependencies.
install :: NonEmpty PkgName -> RIO Env ()
install pkgs = do
  ss <- asks settings
  if not $ switch ss DeleteMakeDeps
    then install' pkgs
    else do -- `-a` was used.
      orphansBefore <- liftIO orphans
      install' pkgs
      orphansAfter <- liftIO orphans
      let makeDeps = nes $ orphansAfter S.\\ orphansBefore
      traverse_ (\mds -> liftIO (notify ss removeMakeDepsAfter_1) *> removePkgs mds) makeDeps

install' :: NonEmpty PkgName -> RIO Env ()
install' pkgs = do
  rpstry   <- asks repository
  ss       <- asks settings
  unneeded <- bool
              (pure S.empty)
              (S.fromList . catMaybes <$> liftIO (traverseConcurrently Par' isInstalled $ toList pkgs))
              $ shared ss NeededOnly
  let !pkgs' = S.fromList $ NEL.toList pkgs
  if shared ss NeededOnly && unneeded == pkgs'
    then warn ss install_2
    else do
      let (ignored, notIgnored) = S.partition (`S.member` ignoresOf ss) pkgs'
      installAnyway <- confirmIgnored ignored
      case nes $ (notIgnored <> installAnyway) S.\\ unneeded of
        Nothing        -> warn ss install_2
        Just toInstall -> do
          traverse_ (report yellow reportUnneededPackages_1) . NEL.nonEmpty
            $ toList unneeded
          (nons, toBuild) <- liftMaybeM (Failure connectFailure_1) . liftIO
            $ aurLookup (managerOf ss) toInstall
          pkgbuildDiffs toBuild
          traverse_ (report red reportNonPackages_1) . NEL.nonEmpty $ toList nons
          let !explicits = bool (S.map (\b -> b { bIsExplicit = True }) toBuild) toBuild
                $ switch ss AsDeps
          case nes explicits of
            Nothing       -> throwM $ Failure install_2
            Just toBuild' -> do
              notify ss install_5 *> hFlush stdout
              allPkgs <- if switch ss SkipDepCheck
                           then pure . (:| []) $ NEL.map FromAUR toBuild'
                           else depsToInstall rpstry toBuild'
              let (repoPkgs, buildPkgs) = second uniquePkgBase $ partitionPkgs allPkgs
              unless (switch ss NoPkgbuildCheck)
                $ traverse_ (traverse_ analysePkgbuild) buildPkgs
              reportPkgsToInstall repoPkgs buildPkgs
              unless (switch ss DryRun) . withOkay ss install_3 install_4 $ do
                traverse_ repoInstall $ NEL.nonEmpty repoPkgs
                let !mbuildPkgs = NEL.nonEmpty buildPkgs
                traverse_ (liftIO . storePkgbuilds . fold1) mbuildPkgs
                traverse_ buildAndInstall mbuildPkgs

-- | Give anything that was installed as a dependency the /Install Reason/ of
-- "Installed as a dependency for another package".
annotateDeps :: NonEmpty Buildable -> IO ()
annotateDeps bs = unless (null bs') . void . pacmanSuccess
  $ ["-D", "--asdeps"] <> asFlag (map bName bs')
  where
    bs' :: [Buildable]
    bs' = NEL.filter (not . bIsExplicit) bs

-- | Reduce a list of candidate packages to build, such that there is only one
-- instance of each "Package Base". This will ensure that split packages will
-- only be built once each. Precedence is given to packages that actually
-- match the base name (e.g. llvm50 vs llvm50-libs).
uniquePkgBase :: [NonEmpty Buildable] -> [NonEmpty Buildable]
uniquePkgBase bs = mapMaybe g bs
  where
    bs' :: [Buildable]
    bs' = foldMap NEL.toList bs

    g :: NonEmpty Buildable -> Maybe (NonEmpty Buildable)
    g = NEL.nonEmpty . L.nub . NEL.filter (\b -> bName b `S.member` goods)

    f :: Buildable -> Buildable -> Buildable
    f a b | bName a == bBase a = a
          | bName b == bBase b = b
          | otherwise = a

    goods :: Set PkgName
    goods = S.fromList . map bName . M.elems . M.fromListWith f $ map (bBase &&& id) bs'

confirmIgnored :: Set PkgName -> RIO Env (Set PkgName)
confirmIgnored (toList -> ps) = do
  ss <- asks settings
  S.fromList <$> filterM (liftIO . optionalPrompt ss . confirmIgnored_1) ps

-- | The nested `NonEmpty`s represent the package "hierarchy", namely, what can
-- be built/installed before what.
depsToInstall :: Repository -> NonEmpty Buildable -> RIO Env (NonEmpty (NonEmpty Package))
depsToInstall repo bs = resolveDeps repo $ NEL.map FromAUR bs

repoInstall :: NonEmpty Prebuilt -> RIO Env ()
repoInstall ps = do
  pacOpts <- asks (asFlag . commonConfigOf . settings)
  liftIO . pacman $ ["-S", "--asdeps"] <> pacOpts <> asFlag (NEL.map pName ps)

buildAndInstall :: NonEmpty (NonEmpty Buildable) -> RIO Env ()
buildAndInstall bss = do
  ss    <- asks settings
  let !pth = either id id . cachePathOf $ commonConfigOf ss
      !allsource = S.member AllSource . makepkgFlagsOf $ buildConfigOf ss
  cache <- liftIO $ cacheContents pth
  when allsource $ notify ss buildPackages_2
  traverse_ (f cache) bss
  when allsource $ do
    let !allsourcePath = fromMaybe srcPkgStore . allsourcePathOf $ buildConfigOf ss
    notify ss $ buildPackages_3 allsourcePath
  where
    -- TODO There is a weird edge case (which might be impossible anyway) where
    -- `built` and the `traverse_` line below don't run, but `annotateDeps` is
    -- called anyway. There is definitely a better way to manage the `NonEmpty`s
    -- here.
    f :: Cache -> NonEmpty Buildable -> RIO Env ()
    f (Cache cache) bs = do
      ss <- asks settings
      let (ps, cached) = fmapEither g $ NEL.toList bs

          -- | If we used @--force@, then take the package as-is. Otherwise, try
          -- to look it up in the package cache. If we find a match, we don't
          -- need to build it.
          g :: Buildable -> Either Buildable PackagePath
          g b = case bToSP b `M.lookup` cache of
            Just pp | not (switch ss ForceBuilding) -> Right pp
            _                                       -> Left b

      built <- traverse buildPackages $ NEL.nonEmpty ps
      traverse_ installPkgFiles $ (built <> Just cached) >>= NEL.nonEmpty
      liftIO $ annotateDeps bs

------------
-- REPORTING
------------
-- | Display dependencies. The result of @-Ad@.
displayPkgDeps :: NonEmpty PkgName -> RIO Env ()
displayPkgDeps ps = do
  rpstry <- asks repository
  ss <- asks settings

  let f :: NonEmpty Buildable -> RIO Env ()
      f = depsToInstall rpstry >=> reportDeps (switch ss LowVerbosity) . partitionPkgs

  (_, goods) <- liftMaybeM (Failure connectFailure_1) . liftIO $ aurLookup (managerOf ss) ps
  traverse_ f $ nes goods
  where
    reportDeps :: Bool -> ([Prebuilt], [NonEmpty Buildable]) -> RIO Env ()
    reportDeps True  = liftIO . uncurry reportListOfDeps
    reportDeps False = uncurry reportPkgsToInstall

reportPkgsToInstall :: [Prebuilt] -> [NonEmpty Buildable] -> RIO Env ()
reportPkgsToInstall rps bps = do
  let (explicits, ds) = L.partition bIsExplicit $ foldMap NEL.toList bps
  f reportPkgsToInstall_1 $ map pName rps
  f reportPkgsToInstall_3 $ map bName ds
  f reportPkgsToInstall_2 $ map bName explicits
  where
    f :: (Language -> Doc AnsiStyle) -> [PkgName] -> RIO Env ()
    f m xs = traverse_ (report green m) . NEL.nonEmpty $ L.sort xs

reportListOfDeps :: [Prebuilt] -> [NonEmpty Buildable] -> IO ()
reportListOfDeps rps bps = f (map pName rps) *> f (map bName $ foldMap NEL.toList bps)
  where
    f :: [PkgName] -> IO ()
    f = traverse_ putTextLn . L.sort . map pnName

pkgbuildDiffs :: Set Buildable -> RIO Env ()
pkgbuildDiffs ps = asks settings >>= check
  where
    check :: Settings -> RIO Env ()
    check ss | not $ switch ss DiffPkgbuilds = pure ()
             | otherwise = traverse_ displayDiff ps

    displayDiff :: Buildable -> RIO Env ()
    displayDiff p = do
      ss <- asks settings
      let pn = bName p
      isStored <- liftIO $ hasPkgbuildStored pn
      if not isStored
        then warn ss $ reportPkgbuildDiffs_1 pn
        else liftIO $ do
          setCurrentDirectory "/tmp"
          let new = "/tmp/new.pb"
          writeFileBinary new . pkgbuild $ bPkgbuild p
          warn ss $ reportPkgbuildDiffs_3 pn
          diff ss (pkgbuildPath pn) new
