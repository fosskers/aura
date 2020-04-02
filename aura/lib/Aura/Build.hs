{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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
import           Aura.Utils
import           Control.Monad.Trans.Except
import           Data.Hashable (hash)
import           Data.Witherable.Class (wither)
import           RIO
import           RIO.Directory (setCurrentDirectory)
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           RIO.Time
import           System.Path
import           System.Path.IO
import           System.Process.Typed

---

srcPkgStore :: Path Absolute
srcPkgStore = fromAbsoluteFilePath "/var/cache/aura/src"

-- | Expects files like: \/var\/cache\/pacman\/pkg\/*.pkg.tar.xz
installPkgFiles :: NonEmpty PackagePath -> RIO Env ()
installPkgFiles files = do
  ss <- asks settings
  liftIO $ checkDBLock ss
  liftIO . pacman $ ["-U"] <> map (T.pack . toFilePath . ppPath) (toList files) <> asFlag (commonConfigOf ss)

-- | All building occurs within temp directories,
-- or in a location specified by the user with flags.
buildPackages :: NonEmpty Buildable -> RIO Env (NonEmpty PackagePath)
buildPackages bs = wither build (NEL.toList bs) >>= maybe bad (pure . fold1) . NEL.nonEmpty
  where bad = throwM $ Failure buildFail_10

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: Buildable -> RIO Env (Maybe (NonEmpty PackagePath))
build p = do
  logDebug $ "Building: " <> display (pnName $ bName p)
  ss <- asks settings
  liftIO $ notify ss (buildPackages_1 (bName p) (langOf ss)) *> hFlush stdout
  result <- build' ss p
  either buildFail (pure . Just) result

-- | Should never throw an IO Exception. In theory all errors
-- will come back via the @Language -> String@ function.
build' :: Settings -> Buildable -> RIO Env (Either Failure (NonEmpty PackagePath))
build' ss b = do
  let pth = buildPathOf $ buildConfigOf ss
  liftIO $ createDirectoryIfMissing True pth
  setCurrentDirectory $ toFilePath pth
  buildDir <- liftIO $ randomDirName b
  liftIO $ createDirectoryIfMissing True buildDir
  setCurrentDirectory $ toFilePath buildDir
  runExceptT $ do
    bs <- ExceptT $ cloneRepo b usr
    lift . setCurrentDirectory $ toFilePath bs
    lift . liftIO $ overwritePkgbuild ss b
    pNames <- ExceptT . liftIO $ makepkg ss usr
    paths  <- liftIO $ traverse (moveToCachePath ss) pNames
    lift . liftIO . when (S.member AllSource . makepkgFlagsOf $ buildConfigOf ss) $
      makepkgSource usr >>= traverse_ moveToSourcePath
    pure paths
  where usr = fromMaybe (User "桜木花道") . buildUserOf $ buildConfigOf ss

-- | Create a temporary directory with a semi-random name based on
-- the `Buildable` we're working with.
randomDirName :: Buildable -> IO (Path Absolute)
randomDirName b = do
  pwd <- getCurrentDirectory
  UTCTime _ dt <- getCurrentTime
  let nh = hash . pnName $ bName b
      vh = hash $ bVersion b
      v  = abs $ nh + vh + floor dt
      dir = T.unpack (pnName $ bName b) <> "-" <> show v
  pure $ pwd </> fromUnrootedFilePath dir

cloneRepo :: Buildable -> User -> RIO Env (Either Failure (Path Absolute))
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
overwritePkgbuild ss p = when (switch ss HotEdit || switch ss UseCustomizepkg) $
  writeFileBinary "PKGBUILD" . pkgbuild $ bPkgbuild p

-- | Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: Failure -> RIO Env (Maybe a)
buildFail (Failure err) = do
  ss <- asks settings
  liftIO . scold ss . err $ langOf ss
  response <- liftIO $ optionalPrompt ss buildFail_6
  bool (throwM $ Failure buildFail_5) (pure Nothing) response

-- | Moves a file to the pacman package cache and returns its location.
moveToCachePath :: Settings -> Path Absolute -> IO PackagePath
moveToCachePath ss p = copy $> PackagePath newName
  where newName = pth </> takeFileName p
        pth     = either id id . cachePathOf $ commonConfigOf ss
        copy    = runProcess . setStderr closed . setStdout closed
                  $ proc "cp" ["--reflink=auto", toFilePath p, toFilePath newName ]

-- | Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: Path Absolute -> IO (Path Absolute)
moveToSourcePath p = renameFile p newName $> newName
  where newName = srcPkgStore </> takeFileName p
