{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

module Aura.Build where

import System.Exit (ExitCode(..))
import Control.Monad (when, unless)
import Data.Maybe (fromJust)
import System.FilePath ((</>), takeFileName)

import Aura.AurConnection (downloadSource)
import Aura.Languages
import Aura.Settings
import Aura.General
import Aura.MakePkg
import Utilities
import Shell
import Zero

type MaybePaths = Maybe [FilePath]

-- Expects files like: /var/cache/pacman/pkg/*.pkg.tar.xz
installPkgFiles :: Settings -> [String] -> [FilePath] -> IO ExitCode
installPkgFiles ss pacOpts files = pacman ss $ ["-U"] ++ pacOpts ++ files

-- All building occurs within temp directories in the package cache.
buildPackages :: Settings -> [AURPkg] -> IO MaybePaths
buildPackages _ [] = return Nothing
buildPackages ss pkgs = inDir cache $ buildPackages' ss (Just []) pkgs
    where cache = cachePathOf ss

-- Handles the building of Packages.
-- Assumed: All pacman and AUR dependencies are already installed.
buildPackages' :: Settings -> MaybePaths -> [AURPkg] -> IO MaybePaths
buildPackages' _ builtPs [] = return builtPs  -- Done recursing!
buildPackages' settings builtPs pkgs@(p:ps) = do
  notify settings (flip buildPackagesMsg1 $ pkgNameOf p)
  results <- withTempDir (pkgNameOf p) (build settings p)
  case results of
    Right pkg   -> buildPackages' settings ((pkg :) `fmap` builtPs) ps
    Left errors -> buildFail settings builtPs pkgs errors
        
-- Kinda ugly.
-- Perform the actual build. Fails elegantly when build fails occur.
build :: Settings -> AURPkg -> IO (Either ErrMsg FilePath)
build ss pkg = do
  currDir <- pwd
  getSourceCode (pkgNameOf pkg) user currDir
  when (mayHotEdit ss) $ overwritePB pkg
  (exitStatus,pkgName,output) <- makepkg' user
  if didProcessFail exitStatus
     then return $ Left output
     else do
       path <- moveToCache (cachePathOf ss) pkgName
       cd currDir
       return $ Right path
    where makepkg'   = if toSuppress then makepkgQuiet else makepkgVerbose
          toSuppress = suppressMakepkg ss
          user       = getTrueUser $ environmentOf ss

getSourceCode :: String -> String -> FilePath -> IO ()
getSourceCode pkgName user currDir = do
  chown user currDir []
  tarball   <- downloadSource currDir pkgName
  sourceDir <- decompress tarball
  chown user sourceDir ["-R"]
  cd sourceDir

-- Allow the user to edit the PKGBUILD if they asked to do so.
hotEdit :: Settings -> [AURPkg] -> IO [AURPkg]
hotEdit ss pkgs = withTempDir "hotedit" . flip mapM pkgs $ \p -> do
  answer <- optionalPrompt (mustConfirm ss) (msg p)
  if not answer
     then return p
     else do
       let filename = pkgNameOf p ++ "-PKGBUILD"
       writeFile filename $ pkgbuildOf p
       openEditor editor filename
       new <- readFile filename
       return $ AURPkg (pkgNameOf p) (versionOf p) new
    where msg p  = checkHotEditMsg1 (langOf ss) $ pkgNameOf p
          editor = getEditor $ environmentOf ss

overwritePB :: AURPkg -> IO ()
overwritePB pkg = writeFile "PKGBUILD" $ pkgbuildOf pkg

-- Inform the user that building failed. Ask them if they want to
-- continue installing previous packages that built successfully.
-- BUG: This prompting ignores `--noconfirm`.
buildFail :: Settings -> MaybePaths -> [AURPkg] -> ErrMsg -> IO MaybePaths
buildFail _ _ [] _ = return Nothing
buildFail settings builtPs (p:ps) errors = do
  scold settings (flip buildFailMsg1 (show p))
  when (suppressMakepkg settings) (displayBuildErrors settings errors)
  unless (null ps) $ printList red cyan (buildFailMsg2 lang) (map pkgNameOf ps)
  (return $ fromJust builtPs) ?>>= \bps -> do
      printList yellow cyan (buildFailMsg3 lang) $ map takeFileName bps
      yesNoPrompt (buildFailMsg4 lang) ?>> return builtPs
    where lang = langOf settings

-- If the user wasn't running Aura with `-x`, then this will
-- show them the suppressed makepkg output. 
displayBuildErrors :: Settings -> ErrMsg -> IO ()
displayBuildErrors settings errors = do
  putStrA red (displayBuildErrorsMsg1 $ langOf settings)
  timedMessage 1000000 ["3.. ","2.. ","1..\n"]
  putStrLn errors

-- Moves a file to the pacman package cache and returns its location.
moveToCache :: FilePath -> FilePath -> IO FilePath
moveToCache cachePath pkg = mv pkg newName >> return newName
    where newName = cachePath </> pkg
