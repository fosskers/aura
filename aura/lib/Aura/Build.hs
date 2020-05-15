{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- |
-- Module    : Aura.Build
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Agnostically builds packages, regardless of original source.

module Aura.Build
  ( installPkgFiles
  , buildPackages
  , srcPkgStore
  , vcsStore
  ) where

import           Aura.Core
import           Aura.IO
import           Aura.Languages
import           Aura.MakePkg
import           Aura.Packages.AUR (clone)
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.Shell (chown)
import           Aura.Types
import           Aura.Utils (edit)
import           Control.Monad.Trans.Except
import           Data.Hashable (hash)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as NEL
import           RIO.Partial (fromJust)
import qualified RIO.Set as S
import qualified RIO.Text as T
import           RIO.Time
import           System.Process.Typed

---

-- | Storage location for "source" packages built with @--allsource@.
-- Can be overridden in config or with @--allsourcepath@.
srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- | Storage/build location for VCS packages like @cool-retroterm-git@. Some of
-- these packages are quite large (e.g. kernels), and so recloning them in their
-- entirety upon each @-Au@ is wasteful.
vcsStore :: FilePath
vcsStore = "/var/cache/aura/vcs"

-- | Expects files like: \/var\/cache\/pacman\/pkg\/*.pkg.tar.xz
installPkgFiles :: NonEmpty PackagePath -> RIO Env ()
installPkgFiles files = do
  ss <- asks settings
  liftIO $ checkDBLock ss
  liftIO . pacman $ ["-U"] <> map (T.pack . ppPath) (toList files) <> asFlag (commonConfigOf ss)

-- | All building occurs within temp directories,
-- or in a location specified by the user with flags.
buildPackages :: NonEmpty Buildable -> RIO Env [PackagePath]
buildPackages bs = mapMaybeA build (NEL.toList bs) >>= \case
  [] -> throwM $ Failure buildFail_10
  built -> pure $ concat built

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: Buildable -> RIO Env (Maybe [PackagePath])
build p = do
  logDebug $ "Building: " <> display (pnName $ bName p)
  ss <- asks settings
  notify ss (buildPackages_1 $ bName p) *> hFlush stdout
  result <- build' p
  either buildFail (pure . Just) result

-- | Should never throw an IO Exception. In theory all errors
-- will come back via the @Language -> String@ function.
--
-- If the package is a VCS package (i.e. ending in -git, etc.), it will be built
-- and stored in a separate, deterministic location to prevent repeated clonings
-- during subsequent builds.
--
-- If `--allsource` was given, then the package isn't actually built.
-- Instead, a @.src.tar.gz@ file is produced and copied to `srcPkgStore`.
build' :: Buildable -> RIO Env (Either Failure [PackagePath])
build' b = do
  ss <- asks settings
  let !isDevel = isDevelPkg $ bName b
      !pth | isDevel = fromMaybe vcsStore . vcsPathOf $ buildConfigOf ss
           | otherwise = fromMaybe defaultBuildDir . buildPathOf $ buildConfigOf ss
      !usr = fromMaybe (User "UNKNOWN") . buildUserOf $ buildConfigOf ss
  createDirectoryIfMissing True pth
  setCurrentDirectory pth
  buildDir <- liftIO $ getBuildDir b
  createDirectoryIfMissing True buildDir
  setCurrentDirectory buildDir
  runExceptT $ do
    bs <- ExceptT $ do
      let !dir = buildDir </> T.unpack (pnName $ bName b)
      pulled <- doesDirectoryExist dir
      bool (cloneRepo b usr) (pure $ Right dir) pulled
    setCurrentDirectory bs
    when isDevel $ ExceptT pullRepo
    liftIO $ overwritePkgbuild ss b
    liftIO $ overwriteInstall ss
    liftIO $ overwritePatches ss
    if S.member AllSource . makepkgFlagsOf $ buildConfigOf ss
      then do
        let !allsourcePath = fromMaybe srcPkgStore . allsourcePathOf $ buildConfigOf ss
        liftIO (makepkgSource usr >>= traverse_ (moveToSourcePath allsourcePath)) $> []
      else do
        pNames <- ExceptT . liftIO . fmap (fmap NEL.toList) $ makepkg ss usr
        liftIO $ traverse (moveToCachePath ss) pNames

getBuildDir :: Buildable -> IO FilePath
getBuildDir b
  | isDevelPkg $ bName b = vcsBuildDir $ bName b
  | otherwise = randomDirName b

vcsBuildDir :: PkgName -> IO FilePath
vcsBuildDir (PkgName pn) = do
  pwd <- getCurrentDirectory
  pure $ pwd </> T.unpack pn

-- | Create a temporary directory with a semi-random name based on
-- the `Buildable` we're working with.
randomDirName :: Buildable -> IO FilePath
randomDirName b = do
  pwd <- getCurrentDirectory
  UTCTime _ dt <- getCurrentTime
  let nh = hash . pnName $ bName b
      vh = hash $ bVersion b
      v  = abs $ nh + vh + floor dt
      dir = T.unpack (pnName $ bName b) <> "-" <> show v
  pure $ pwd </> dir

cloneRepo :: Buildable -> User -> RIO Env (Either Failure FilePath)
cloneRepo pkg usr = do
  currDir <- liftIO getCurrentDirectory
  logDebug $ "Currently in: " <> displayShow currDir
  scriptsDir <- liftIO $ chown usr currDir [] *> clone pkg
  case scriptsDir of
    Nothing -> pure . Left . Failure . buildFail_7 $ bName pkg
    Just sd -> chown usr sd ["-R"] $> Right sd

-- | Assuming that we're already in a VCS-based package's build folder,
-- just pull the latest instead of cloning.
pullRepo :: RIO Env (Either Failure ())
pullRepo = do
  ec <- runProcess . setStderr closed . setStdout closed $ proc "git" ["pull"]
  case ec of
    ExitFailure _ -> pure . Left $ Failure buildFail_12
    ExitSuccess   -> pure $ Right ()

-- | Edit the PKGBUILD in-place, if the user wants to.
overwritePkgbuild :: Settings -> Buildable -> IO ()
overwritePkgbuild ss b = when (switch ss HotEdit) . liftIO $ do
  ans <- optionalPrompt ss (hotEdit_1 $ bName b)
  when ans $ edit (editorOf ss) "PKGBUILD"

-- | Edit the .install file in-place, if the user wants to and it exists.
overwriteInstall :: Settings -> IO ()
overwriteInstall ss = when (switch ss HotEdit) . liftIO $ do
  files <- getCurrentDirectory >>= listDirectory
  case L.find ((== ".install") . takeFileName) files of
    Nothing -> pure ()
    Just _  -> do
      ans <- optionalPrompt ss hotEdit_2
      when ans $ edit (editorOf ss) ".install"

-- | Edit the all .patch files, if the user wants to and some exist.
overwritePatches :: Settings -> IO ()
overwritePatches ss = when (switch ss HotEdit) . liftIO $ do
  files <- getCurrentDirectory >>= listDirectory
  let !patches = filter ((== ".patch") . takeExtension) files
  traverse_ f patches
  where
    f :: FilePath -> IO ()
    f p = do
      ans <- optionalPrompt ss $ hotEdit_3 p
      when ans $ edit (editorOf ss) p

-- | Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: Failure -> RIO Env (Maybe a)
buildFail (Failure err) = do
  ss <- asks settings
  scold ss err
  withOkay ss buildFail_6 buildFail_5 $ pure Nothing

-- | Moves a file to the pacman package cache and returns its location.
moveToCachePath :: Settings -> FilePath -> IO PackagePath
moveToCachePath ss p = copy $> fromJust (packagePath newName)
  where newName = pth </> takeFileName p
        pth     = either id id . cachePathOf $ commonConfigOf ss
        copy    = runProcess . setStderr closed . setStdout closed
                  $ proc "cp" ["--reflink=auto", p, newName]

-- | Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: FilePath -> FilePath -> IO FilePath
moveToSourcePath allsourcePath p = do
  createDirectoryIfMissing True allsourcePath
  copy $> newName
  where
    newName = allsourcePath </> takeFileName p
    copy    = runProcess . setStderr closed . setStdout closed
              $ proc "cp" ["--reflink=auto", p, newName]
