{-# LANGUAGE OverloadedStrings #-}

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

-- Agnostically builds packages. They can be either AUR or ABS.

module Aura.Build
  ( installPkgFiles
  , buildPackages
  ) where

import           Aura.Cache (defaultPackageCache)
import           Aura.Core
import           Aura.Languages
import           Aura.MakePkg
import           Aura.Monad.Aura
import           Aura.Pacman (pacman)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import           Data.Bitraversable (bitraverse)
import qualified Data.Set as S
import qualified Data.Text as T
import           Filesystem.Path (filename)
import           Shelly
import           Utilities

---

srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- | Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: Maybe T.Text -> [T.Text] -> Aura (Either Failure ())
installPkgFiles _ []         = pure $ Right ()
installPkgFiles asDeps files = do
  ask >>= shelly . checkDBLock
  pacman $ ["-U"] <> maybe [] (:[]) asDeps <> files

-- | All building occurs within temp directories in the package cache,
-- or in a location specified by the user with flags.
buildPackages :: [Buildable] -> Aura (Either Failure [FilePath])
buildPackages = fmap (fmap concat . sequenceA) . traverse build

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: Buildable -> Aura (Either Failure [FilePath])
build p = do
  ss     <- ask
  notify $ buildPackages_1 (baseNameOf p) (langOf ss)
  result <- shelly $ build' ss p
  either (buildFail p) (pure . Right) result

-- | Should never throw a Shelly Exception. In theory all errors
-- will come back via the @Language -> String@ function.
build' :: Settings -> Buildable -> Sh (Either Failure [FilePath])
build' ss p = do
  let pth = fromMaybe defaultPackageCache $ buildPathOf (buildConfigOf ss) <|> cachePathOf (commonConfigOf ss)
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
          paths <- traverse (moveToCachePath ss) pns
          when (S.member AllSource . makepkgFlagsOf $ buildConfigOf ss) $
            makepkgSource user >>= traverse_ moveToSourcePath
          pure paths

getBuildScripts :: Buildable -> User -> Sh (Either Failure FilePath)
getBuildScripts pkg user = do
  currDir <- toTextIgnore <$> pwd
  scriptsDir <- chown user currDir [] *> liftIO (buildScripts pkg (T.unpack currDir))
  case scriptsDir of
    Nothing -> pure . failure . buildFail_7 $ baseNameOf pkg
    Just sd -> do
      let sd' = T.pack sd
      chown user sd' ["-R"]
      pure . Right $ fromText sd'

-- | The user may have edited the original PKGBUILD. If they have, we need to
-- overwrite what's been downloaded before calling `makepkg`.
overwritePkgbuild :: Settings -> Buildable -> Sh ()
overwritePkgbuild ss p = when (switch ss HotEdit || switch ss UseCustomizepkg) $
  writefile "PKGBUILD" . _pkgbuild $ pkgbuildOf p

-- | Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: Buildable -> Failure -> Aura (Either Failure [a])
buildFail p (Failure err) = do
  ss <- ask
  scold $ buildFail_1 (baseNameOf p) (langOf ss)
  scold . err $ langOf ss
  response <- optionalPrompt ss buildFail_6
  pure $ bool (failure buildFail_5) (Right []) response

-- | Moves a file to the pacman package cache and returns its location.
moveToCachePath :: Settings -> FilePath -> Sh FilePath
moveToCachePath ss p = cp p newName $> newName
  where newName = pth </> filename p
        pth     = fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss

-- | Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: FilePath -> Sh FilePath
moveToSourcePath p = mv p newName $> newName
  where newName = srcPkgStore </> p
