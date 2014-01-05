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

import System.FilePath ((</>), takeFileName)
import Control.Monad   (when, void, join)
import Control.Applicative ((<*>), pure)

import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.MakePkg
import Aura.Utils
import Aura.Core

import Utilities
import Shell

---

-- TODO should this be elsewhere
srcPkgStore :: FilePath
srcPkgStore = "/var/cache/aura/src"

-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: [String] -> [FilePath] -> Aura ()
installPkgFiles _ []          = return ()
installPkgFiles pacOpts files = checkDBLock >> pacman (["-U"] ++ pacOpts ++ files)

-- All building occurs within temp directories in the package cache,
-- or in a location specified by the user with flags.
buildPackages :: [Buildable] -> Aura [FilePath]
buildPackages []   = return []
buildPackages pkgs = ask >>= \ss -> do
  let buildPath = buildPathOf ss
  result <- liftIO $ inDir buildPath (runAura (build [] pkgs) ss)
  wrap result

-- Handles the building of Packages. Fails nicely.
-- Assumed: All dependencies are already installed.
build :: [FilePath] -> [Buildable] -> Aura [FilePath]
build built []       = return $ filter notNull built
build built ps@(p:_) = do
  notify $ buildPackages_1 pn
  (paths,rest) <- catch (withTempDir pn (build' ps)) (buildFail built ps)
  build (paths ++ built) rest
      where pn = baseNameOf p
        
-- | Perform the actual build.
build' :: [Buildable] -> Aura ([FilePath],[Buildable])
build' []     = failure "build' : You should never see this."
build' (p:ps) = ask >>= \ss -> do
  let user = buildUserOf ss
  curr     <- liftIO pwd
  getSourceCode p user curr
  overwritePkgbuild p
  pNames <- join (makepkg <*> pure user)
--  pNames <- makepkg >>= \f -> f user  -- Which is better?
  paths  <- moveToBuildPath pNames
  when (keepSource ss) $ makepkgSource user True >>= void . moveToSourcePath
  liftIO $ cd curr
  return (paths,ps)

getSourceCode :: Buildable -> String -> FilePath -> Aura ()
getSourceCode pkg user currDir = liftIO $ do
  chown user currDir []
  sourceDir <- source pkg currDir
  chown user sourceDir ["-R"]
  cd sourceDir

overwritePkgbuild :: Buildable -> Aura ()
overwritePkgbuild p = asks (\ss -> any ($ ss) checks) >>=
                      flip when (liftIO . writeFile "PKGBUILD" . pkgbuildOf $ p)
    where checks = [mayHotEdit,useCustomizepkg]

-- Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: [FilePath] -> [Buildable] -> String -> Aura ([FilePath],[Buildable])
buildFail _ [] _ = failure "buildFail : You should never see this message."
buildFail built (p:ps) errors = asks langOf >>= \lang -> do
  scold $ buildFail_1 (baseNameOf p)
  displayBuildErrors errors
--  printList red cyan (buildFail_2 lang) (map pkgBase ps)
--  printList yellow cyan (buildFail_3 lang) $ map takeFileName built
  response <- optionalPrompt buildFail_6
  if response
     then return ([],[])
     else scoldAndFail buildFail_5

-- If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output. 
displayBuildErrors :: Error -> Aura ()
displayBuildErrors errors = ask >>= \ss -> when (suppressMakepkg ss) $ do
  putStrA red (displayBuildErrors_1 $ langOf ss)
  liftIO (timedMessage 1000000 ["3.. ","2.. ","1..\n"] >> putStrLn errors)

-- Moves a file to the pacman package cache and returns its location.
moveToBuildPath :: [FilePath] -> Aura [FilePath]
moveToBuildPath []     = return []
moveToBuildPath (p:ps) = do
  newName <- ((</> p) . buildPathOf) <$> ask
  liftIO $ mv p newName
  (newName :) <$> moveToBuildPath ps

-- Moves a file to the aura src package cache and returns its location.
moveToSourcePath :: [FilePath] -> Aura [FilePath]
moveToSourcePath []     = return []
moveToSourcePath (p:ps) = do
  let newName = srcPkgStore </> p
  liftIO $ mv p newName
  (newName :) <$> moveToSourcePath ps
