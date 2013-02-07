{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

module Aura.Build
    ( installPkgFiles
    , buildPackages
    , hotEdit ) where

import System.FilePath ((</>), takeFileName)
import Control.Monad   (liftM, forM, when)

import Aura.AurConnection (downloadSource)
import Aura.Pacman        (pacman)
import Aura.Settings.Base
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.MakePkg
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities
import Shell

---

-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: [String] -> [FilePath] -> Aura ()
installPkgFiles pacOpts files = pacman $ ["-U"] ++ pacOpts ++ files

-- All building occurs within temp directories in the package cache.
buildPackages :: [AURPkg] -> Aura [FilePath]
buildPackages []   = return []
buildPackages pkgs = ask >>= \ss -> do
  let cache = cachePathOf ss
  result <- liftIO $ inDir cache (runAura (build [] pkgs) ss)
  wrap result

-- Handles the building of Packages. Fails nicely.
-- Assumed: All pacman and AUR dependencies are already installed.
build :: [FilePath] -> [AURPkg] -> Aura [FilePath]
build built []       = return $ filter notNull built
build built ps@(p:_) = do
  notify (flip buildPackagesMsg1 pn)
  (paths,rest) <- catch (withTempDir pn (build' ps)) (buildFail built ps)
  build (paths ++ built) rest
      where pn = pkgNameOf p
        
-- Perform the actual build.
build' :: [AURPkg] -> Aura ([FilePath],[AURPkg])
build' []     = failure "build' : You should never see this."
build' (p:ps) = ask >>= \ss -> do
  let makepkg'   = if toSuppress then makepkgQuiet else makepkgVerbose
      toSuppress = suppressMakepkg ss
      user       = getTrueUser $ environmentOf ss
  curr <- liftIO pwd
  getSourceCode (pkgNameOf p) user curr
  overwritePkgbuild p
  pNames <- makepkg' user
  paths  <- moveToCache pNames
  liftIO $ cd curr
  return (paths,ps)

getSourceCode :: String -> String -> FilePath -> Aura ()
getSourceCode pkgName user currDir = liftIO $ do
  chown user currDir []
  tarball   <- downloadSource currDir pkgName
  sourceDir <- decompress tarball
  chown user sourceDir ["-R"]
  cd sourceDir

-- Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: [AURPkg] -> Aura [AURPkg]
hotEdit pkgs = ask >>= \ss ->
  withTempDir "hotedit" . forM pkgs $ \p -> do
    let msg = flip checkHotEditMsg1 . pkgNameOf
    answer <- optionalPrompt (msg p)
    if not answer
       then return p
       else do
         newPB <- liftIO $ do
                    let filename = pkgNameOf p ++ "-PKGBUILD"
                        editor   = getEditor $ environmentOf ss
                    writeFile filename $ pkgbuildOf p
                    openEditor editor filename
                    readFile filename
         newNS <- globals (pkgNameOf p) newPB  -- Reparse PKGBUILD.
         return $ AURPkg (pkgNameOf p) (versionOf p) newPB newNS

overwritePkgbuild :: AURPkg -> Aura ()
overwritePkgbuild p = (mayHotEdit `liftM` ask) >>= check
    where check True  = liftIO . writeFile "PKGBUILD" . pkgbuildOf $ p
          check False = return ()

-- Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
buildFail :: [FilePath] -> [AURPkg] -> String -> Aura ([FilePath],[AURPkg])
buildFail _ [] _ = failure "buildFail : You should never see this message."
buildFail built (p:ps) errors = ask >>= \ss -> do
  let lang = langOf ss
  scold (flip buildFailMsg1 (show p))
  displayBuildErrors errors
  printList red cyan (buildFailMsg2 lang) (map pkgNameOf ps)
  printList yellow cyan (buildFailMsg3 lang) $ map takeFileName built
  if null built
     then return ([],[])
     else do
       response <- optionalPrompt buildFailMsg4
       if response
          then return ([],[])
          else scoldAndFail buildFailMsg5

-- If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output. 
displayBuildErrors :: ErrMsg -> Aura ()
displayBuildErrors errors = ask >>= \ss -> when (suppressMakepkg ss) $ do
  putStrA red (displayBuildErrorsMsg1 $ langOf ss)
  liftIO (timedMessage 1000000 ["3.. ","2.. ","1..\n"] >> putStrLn errors)

-- Moves a file to the pacman package cache and returns its location.
moveToCache :: [FilePath] -> Aura [FilePath]
moveToCache []     = return []
moveToCache (p:ps) = do
  newName <- ((</> p) . cachePathOf) `liftM` ask
  liftIO (mv p newName)
  (newName :) `liftM` moveToCache ps
