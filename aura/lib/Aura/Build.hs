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
import           Control.Monad.Trans.Except
import           Data.Hashable (hash)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.NonEmpty as NEL
import           RIO.Partial (fromJust)
import qualified RIO.Set as S
import qualified RIO.Text as T
import           RIO.Time
import           System.Process.Typed

---

srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

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
  result <- build' ss p
  either buildFail (pure . Just) result

-- | Should never throw an IO Exception. In theory all errors
-- will come back via the @Language -> String@ function.
--
-- If `--allsource` was given, then the package isn't actually built.
-- Instead, a @.src.tar.gz@ file is produced and copied to `srcPkgStore`.
build' :: Settings -> Buildable -> RIO Env (Either Failure [PackagePath])
build' ss b = do
  let !pth = fromMaybe defaultBuildDir . buildPathOf $ buildConfigOf ss
  createDirectoryIfMissing True pth
  setCurrentDirectory pth
  buildDir <- liftIO $ randomDirName b
  createDirectoryIfMissing True buildDir
  setCurrentDirectory buildDir
  runExceptT $ do
    bs <- ExceptT $ cloneRepo b usr
    setCurrentDirectory bs
    liftIO $ overwritePkgbuild ss b
    if S.member AllSource . makepkgFlagsOf $ buildConfigOf ss
      then liftIO (makepkgSource usr >>= traverse_ moveToSourcePath) $> []
      else do
        pNames <- ExceptT . liftIO . fmap (fmap NEL.toList) $ makepkg ss usr
        paths  <- liftIO $ traverse (moveToCachePath ss) pNames
        pure paths
  where
    -- | We expect `buildUserOf` to always return a `Just` at this point. Aura
    -- should have failed at startup otherwise.
    usr :: User
    usr = fromMaybe (User "UNKNOWN") . buildUserOf $ buildConfigOf ss

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

-- | The user may have edited the original PKGBUILD. If they have, we need to
-- overwrite what's been downloaded before calling `makepkg`.
overwritePkgbuild :: Settings -> Buildable -> IO ()
overwritePkgbuild ss p =
  when (switch ss HotEdit) . writeFileBinary "PKGBUILD" . pkgbuild $ bPkgbuild p

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
moveToSourcePath :: FilePath -> IO FilePath
moveToSourcePath p = renameFile p newName $> newName
  where newName = srcPkgStore </> takeFileName p
