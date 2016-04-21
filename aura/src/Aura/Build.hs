{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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
    , buildPackages ) where

import BasicPrelude hiding (FilePath, catch, liftIO, (</>))

import qualified Data.Text as T

import Aura.Colour.Text
import Aura.Core
import Aura.Languages
import Aura.MakePkg
import Aura.Monad.Aura
import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.Utils

import Utilities
import Aura.Shell
import Shelly hiding (liftIO)
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text.IO as IO

---

-- TODO should this be elsewhere
srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: [T.Text] -> [T.Text] -> Aura ()
installPkgFiles _ []          = pure ()
installPkgFiles pacOpts files = checkDBLock *> pacman (["-U"] <> pacOpts <> files)

-- All building occurs within temp directories in the package cache,
-- or in a location specified by the user with flags.
buildPackages :: [Buildable] -> Aura [FilePath]
buildPackages []   = pure []
buildPackages pkgs = ask >>= \ss -> do
  let buildPath = buildPathOf ss
  result <- liftShelly $ inDir buildPath (runAura (build [] pkgs) ss)
  wrap result

-- Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: [FilePath] -> [Buildable] -> Aura [FilePath]
build built []       = pure $ filter (not . F.null)  built
build built ps@(p:_) = do
  notify $ buildPackages_1 pn
  (paths, rest) <- catch (withTempDir (F.fromText pn) (build' ps)) (buildFail built ps)
  build (paths <> built) rest
      where pn = baseNameOf p
        
-- | Perform the actual build.
build' :: [Buildable] -> Aura ([FilePath], [Buildable])
build' []     = failure "build' : You should never see this."
build' (p:ps) = ask >>= \ss -> do
  let user = buildUserOf ss
      quiet = suppressMakepkg ss
      makeflags = makepkgFlagsOf ss
  curr   <- liftShelly pwd
  getBuildScripts p user curr
  overwritePkgbuild p
  pNames <- liftShelly (makepkg quiet user makeflags) >>= wrap
--  pNames <- makepkg >>= \f -> f user  -- Which is better?
  paths  <- moveToCachePath pNames
  when (keepSource ss) $ liftShelly (makepkgSource user) >>= void . moveToSourcePath
  liftShelly $ cd curr
  pure (paths, ps)

getBuildScripts :: Buildable -> T.Text -> FilePath -> Aura ()
getBuildScripts pkg user currDir = do
  scriptsDir <- liftShelly (chown user (toTextIgnore currDir) []) *> buildScripts pkg currDir
  case scriptsDir of
    Nothing -> scoldAndFail (buildFail_7 $ baseNameOf pkg)
    Just sd -> liftShelly $ do
                 chown user (toTextIgnore sd) ["-R"]
                 cd sd
{-}
getBuildScripts pkg user currDir = liftIO $ do
  chown user currDir []
  scriptsDir <- buildScripts pkg currDir
  chown user scriptsDir ["-R"]
  cd scriptsDir
-}

overwritePkgbuild :: Buildable -> Aura ()
overwritePkgbuild p = asks (\ss -> any ($ ss) checks) >>=
                      flip when (liftIO . IO.writeFile "PKGBUILD" . pkgbuildOf $ p)
    where checks = [mayHotEdit, useCustomizepkg]

-- Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: [FilePath] -> [Buildable] -> T.Text -> Aura ([FilePath], [Buildable])
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

-- If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output. 
displayBuildErrors :: Error -> Aura ()
displayBuildErrors errors = ask >>= \ss -> when (suppressMakepkg ss) $ do
  putStrA red (displayBuildErrors_1 $ langOf ss)
  liftIO (timedMessage 1000000 ["3.. ", "2.. ", "1..\n"] *> IO.putStrLn errors)

-- Moves a file to the pacman package cache and returns its location.
moveToCachePath :: [FilePath] -> Aura [FilePath]
moveToCachePath []     = pure []
moveToCachePath (p:ps) = do
  newName <- (</> F.filename p) <$> asks cachePathOf
  liftShelly $ cp p newName
  (newName :) <$> moveToCachePath ps

-- Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: [FilePath] -> Aura [FilePath]
moveToSourcePath []     = pure []
moveToSourcePath (p:ps) = do
  let newName = srcPkgStore </> p
  liftShelly $ mv p newName
  (newName :) <$> moveToSourcePath ps
