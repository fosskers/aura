{-# LANGUAGE OverloadedStrings, TupleSections #-}

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

import           Aura.Colour.Text
import           Aura.Core
import           Aura.Languages
import           Aura.MakePkg
import           Aura.Monad.Aura
import           Aura.Pacman (pacman)
import           Aura.Settings.Base
import           Aura.Utils
import           BasePrelude hiding (catch, FilePath)
import           Data.Bitraversable (bitraverse)
import qualified Data.Text as T
import           Shelly
import           Utilities

---

srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- | Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: [T.Text] -> [T.Text] -> Aura ()
installPkgFiles _ []          = pure ()
installPkgFiles pacOpts files = checkDBLock *> pacman (["-U"] <> pacOpts <> files)

-- | All building occurs within temp directories in the package cache,
-- or in a location specified by the user with flags.
buildPackages :: [Buildable] -> Aura [FilePath]
buildPackages []   = pure []
buildPackages pkgs = ask >>= \ss -> do
  let buildPath = buildPathOf ss
  result <- liftIO $ inDir buildPath (runAura (build [] pkgs) ss)
  wrap result

-- | Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: [FilePath] -> [Buildable] -> Aura [FilePath]
build built []       = pure $ filter (not . T.null . toTextIgnore) built
build built ps@(p:_) = do
  notify $ buildPackages_1 pn
  (paths, rest) <- catch (withTempDir pn (build' ps)) (buildFail built ps)
  build (paths <> built) rest
    where pn = baseNameOf p

-- | Perform the actual build.
build' :: [Buildable] -> Aura ([FilePath], [Buildable])
build' []     = failure "build' : You should never see this."
build' (p:ps) = do
  ss    <- ask
  paths <- shelly $ build'' ss p
  either scoldAndFail (pure . (,ps)) paths

build'' :: Settings -> Buildable -> Sh (Either (Language -> String) [FilePath])
build'' ss p = getBuildScripts p user >>= either (pure . Left) f
  where user = buildUserOf ss
        f bs = do
          cd bs
          overwritePkgbuild ss p
          pNames <- makepkg ss user
          bitraverse pure g pNames
        g pns = do
          paths <- traverse (moveToCachePath ss) pns
          when (keepSource ss) $ (makepkgSource user) >>= traverse_ moveToSourcePath
          pure paths

getBuildScripts :: Buildable -> User -> Sh (Either (Language -> String) FilePath)
getBuildScripts pkg user = do
  currDir <- toTextIgnore <$> pwd
  scriptsDir <- chown user currDir [] *> liftIO (buildScripts pkg (T.unpack currDir))
  case scriptsDir of
    Nothing -> pure . Left . buildFail_7 $ baseNameOf pkg
    Just sd -> do
      let sd' = T.pack sd
      chown user sd' ["-R"]
      pure . Right $ fromText sd'

-- | The user may have edited the original PKGBUILD. If they have, we need to
-- overwrite what's been downloaded before calling `makepkg`.
overwritePkgbuild :: Settings -> Buildable -> Sh ()
overwritePkgbuild ss p = when (mayHotEdit ss || useCustomizepkg ss) $ do
  writefile "PKGBUILD" . T.pack $ pkgbuildOf p

-- | Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: [FilePath] -> [Buildable] -> String -> Aura ([FilePath], [Buildable])
buildFail _ [] _ = failure "buildFail : You should never see this message."
buildFail _ (p:_) errors = do  -- asks langOf >>= \lang -> do
  scold $ buildFail_1 (baseNameOf p)
  displayBuildErrors errors
--  printList red cyan (buildFail_2 lang) (pkgBase <$> ps)
--  printList yellow cyan (buildFail_3 lang) (takeFileName <$> built)
  response <- optionalPrompt buildFail_6
  if response
     then pure ([], [])
     else scoldAndFail buildFail_5

-- | If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output.
displayBuildErrors :: Error -> Aura ()
displayBuildErrors errors = ask >>= \ss -> when (suppressMakepkg ss == BeQuiet) $ do
  putStrA red (displayBuildErrors_1 $ langOf ss)
  liftIO (timedMessage 1000000 ["3.. ", "2.. ", "1..\n"] *> putStrLn errors)

-- | Moves a file to the pacman package cache and returns its location.
moveToCachePath :: Settings -> FilePath -> Sh FilePath
moveToCachePath ss p = mv p newName $> newName
  where newName = cachePathOf ss </> p

-- | Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: FilePath -> Sh FilePath
moveToSourcePath p = mv p newName $> newName
  where newName = srcPkgStore </> p
