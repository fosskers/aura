{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import           Aura.Languages
import           Aura.MakePkg
import           Aura.Packages.AUR (clone)
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           Control.Monad.Trans.Except
import           Data.Generics.Product (field)
import           Data.Semigroup.Foldable (fold1)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import           Data.Witherable.Class (wither)
import           RIO
import           RIO.Directory (setCurrentDirectory)
import qualified RIO.NonEmpty as NEL
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Path
import           System.Path.IO
import           System.Process.Typed
import           System.Random.MWC (GenIO, createSystemRandom, uniform)

---

srcPkgStore :: Path Absolute
srcPkgStore = fromAbsoluteFilePath "/var/cache/aura/src"

-- | Expects files like: \/var\/cache\/pacman\/pkg\/*.pkg.tar.xz
installPkgFiles :: NESet PackagePath -> RIO Env ()
installPkgFiles files = do
  ss <- asks settings
  liftIO $ checkDBLock ss
  liftIO . pacman $ ["-U"] <> map (T.pack . toFilePath . path) (toList files) <> asFlag (commonConfigOf ss)

-- | All building occurs within temp directories,
-- or in a location specified by the user with flags.
buildPackages :: NESet Buildable -> RIO Env (NESet PackagePath)
buildPackages bs = do
  g <- liftIO createSystemRandom
  wither (build g) (toList bs) >>= maybe bad (pure . fold1) . NEL.nonEmpty
  where bad = throwM $ Failure buildFail_10

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: GenIO -> Buildable -> RIO Env (Maybe (NESet PackagePath))
build g p = do
  ss     <- asks settings
  liftIO $ notify ss (buildPackages_1 (p ^. field @"name") (langOf ss)) *> hFlush stdout
  result <- liftIO $ build' ss g p
  either buildFail (pure . Just) result

-- | Should never throw an IO Exception. In theory all errors
-- will come back via the @Language -> String@ function.
build' :: Settings -> GenIO -> Buildable -> IO (Either Failure (NESet PackagePath))
build' ss g b = do
  let pth = buildPathOf $ buildConfigOf ss
  createDirectoryIfMissing True pth
  setCurrentDirectory $ toFilePath pth
  buildDir <- randomDirName g b
  createDirectoryIfMissing True buildDir
  setCurrentDirectory $ toFilePath buildDir
  runExceptT $ do
    bs <- ExceptT $ cloneRepo b usr
    lift . setCurrentDirectory $ toFilePath bs
    lift $ overwritePkgbuild ss b
    pNames <- ExceptT $ makepkg ss usr
    paths  <- lift . fmap NES.fromList . traverse (moveToCachePath ss) $ NES.toList pNames
    lift . when (S.member AllSource . makepkgFlagsOf $ buildConfigOf ss) $
      makepkgSource usr >>= traverse_ moveToSourcePath
    pure paths
  where usr = fromMaybe (User "桜木花道") . buildUserOf $ buildConfigOf ss

-- | Create a temporary directory with a semi-random name based on
-- the `Buildable` we're working with.
randomDirName :: GenIO -> Buildable -> IO (Path Absolute)
randomDirName g b = do
  pwd <- getCurrentDirectory
  v   <- uniform g :: IO Word
  let dir = T.unpack (b ^. field @"name" . field @"name") <> "-" <> show v
  pure $ pwd </> fromUnrootedFilePath dir

cloneRepo :: Buildable -> User -> IO (Either Failure (Path Absolute))
cloneRepo pkg usr = do
  currDir <- getCurrentDirectory
  scriptsDir <- chown usr currDir [] *> clone pkg
  case scriptsDir of
    Nothing -> pure . Left . Failure . buildFail_7 $ pkg ^. field @"name"
    Just sd -> chown usr sd ["-R"] $> Right sd

-- | The user may have edited the original PKGBUILD. If they have, we need to
-- overwrite what's been downloaded before calling `makepkg`.
overwritePkgbuild :: Settings -> Buildable -> IO ()
overwritePkgbuild ss p = when (switch ss HotEdit || switch ss UseCustomizepkg) $
  writeFileBinary "PKGBUILD" $ p ^. field @"pkgbuild" . field @"pkgbuild"

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
