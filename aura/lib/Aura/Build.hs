{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

-- |
-- Module    : Aura.Build
-- Copyright : (c) Colin Woodbury, 2012 - 2018
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
import           BasePrelude hiding (FilePath)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Bitraversable (bitraverse)
import qualified Data.List.NonEmpty as NEL
import           Data.Semigroup.Foldable (fold1)
import qualified Data.Set as S
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import           Data.Witherable (wither)
import           Filesystem.Path (filename)
import           Shelly
import           System.IO (hFlush, stdout)

---

srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- | Expects files like: \/var\/cache\/pacman\/pkg\/*.pkg.tar.xz
installPkgFiles :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet PackagePath -> Eff r ()
installPkgFiles files = do
  ss <- ask
  send . shelly @IO $ checkDBLock ss
  rethrow . pacman $ ["-U"] <> map (toTextIgnore . _pkgpath) (toList files) <> asFlag (commonConfigOf ss)

-- | All building occurs within temp directories,
-- or in a location specified by the user with flags.
buildPackages :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  NonEmptySet Buildable -> Eff r (NonEmptySet PackagePath)
buildPackages = wither build . toList >=> maybe bad (pure . fold1) . NEL.nonEmpty
  where bad = throwError $ Failure buildFail_10

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Buildable -> Eff r (Maybe (NonEmptySet PackagePath))
build p = do
  ss     <- ask
  send $ notify ss (buildPackages_1 (bldNameOf p) (langOf ss)) *> hFlush stdout
  result <- send . shelly @IO $ build' ss p
  either (buildFail p) (pure . Just) result

-- | Should never throw a Shelly Exception. In theory all errors
-- will come back via the @Language -> String@ function.
build' :: Settings -> Buildable -> Sh (Either Failure (NonEmptySet PackagePath))
build' ss p = do
  let pth = buildPathOf $ buildConfigOf ss
  cd pth
  withTmpDir $ \curr -> do
    cd curr
    getBuildScripts p user >>= fmap join . bitraverse pure f
  where user = fromMaybe (User "桜木花道") . buildUserOf $ buildConfigOf ss
        f bs = do
          cd bs
          overwritePkgbuild ss p
          pNames <- makepkg ss user
          bitraverse pure g pNames
        g pns = do
          paths <- fmap NES.fromNonEmpty . traverse (moveToCachePath ss) $ NES.toNonEmpty pns
          when (S.member AllSource . makepkgFlagsOf $ buildConfigOf ss) $
            makepkgSource user >>= traverse_ moveToSourcePath
          pure paths

getBuildScripts :: Buildable -> User -> Sh (Either Failure FilePath)
getBuildScripts pkg user = do
  currDir <- pwd
  scriptsDir <- chown user currDir [] *> clone pkg
  case scriptsDir of
    Nothing -> pure . Left . Failure . buildFail_7 $ bldNameOf pkg
    Just sd -> do
      chown user sd ["-R"]
      pure $ Right sd

-- | The user may have edited the original PKGBUILD. If they have, we need to
-- overwrite what's been downloaded before calling `makepkg`.
overwritePkgbuild :: Settings -> Buildable -> Sh ()
overwritePkgbuild ss p = when (switch ss HotEdit || switch ss UseCustomizepkg) $
  writefile "PKGBUILD" . _pkgbuild $ pkgbuildOf p

-- | Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  Buildable -> Failure -> Eff r (Maybe a)
buildFail p (Failure err) = do
  ss <- ask
  send . scold ss $ buildFail_1 (bldNameOf p) (langOf ss)
  send . scold ss . err $ langOf ss
  response <- send $ optionalPrompt ss buildFail_6
  bool (throwError $ Failure buildFail_5) (pure Nothing) response

-- | Moves a file to the pacman package cache and returns its location.
moveToCachePath :: Settings -> FilePath -> Sh PackagePath
moveToCachePath ss p = cp p newName $> PackagePath newName
  where newName = pth </> filename p
        pth     = either id id . cachePathOf $ commonConfigOf ss

-- | Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: FilePath -> Sh FilePath
moveToSourcePath p = mv p newName $> newName
  where newName = srcPkgStore </> p
