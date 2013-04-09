{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-

Copyright 2012, 2013
Colin Woodbury <colingw@gmail.com>
Nicholas Clarke <nicholas.clarke@sanger.ac.uk>

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

{- | Handles all ABS related functions.
-}
module Aura.Packages.ABS (
   absPkg
  ,absSearchLookup
  ,absSync
  ,filterABSPkgs
  ,repoOf
  ,ABSPkg
  )
where

import Data.List (find)
import Control.Monad    (filterM, liftM, void)
import Text.Regex.PCRE  ((=~))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath

import Aura.Bash
import Aura.Core
import Aura.Languages
import Aura.Monad.Aura
import Aura.Colour.Text
import Aura.Settings.Base
import Aura.Pacman (pacmanOutput)
import Aura.Utils  (entrify)
import Aura.Packages.Repository (filterRepoPkgs)

import Utilities (readFileUTF8, split)

import qualified Aura.Shell as A (quietShellCmd, shellCmd)  -- Aura - Has failure checks
import qualified Shell      as S (quietShellCmd)  -- IO   - Doesn't

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String String VersionDemand Pkgbuild Namespace

instance Package ABSPkg where
  package = absPkg
  pkgNameOf (ABSPkg n _ _ _ _) = n
  versionOf (ABSPkg _ _ v _ _) = v

instance Buildable ABSPkg where
  pkgbuildOf  (ABSPkg _ _ _ p _)  = p
  namespaceOf (ABSPkg _ _ _ _ ns) = ns
  source p fp = do
      let loc = absBasePath </> repoOf p </> pkgNameOf p
      S.quietShellCmd "cp" ["-R",loc,fp]
      return $ fp </> pkgNameOf p
  rewrap (ABSPkg n r v p _) ns = ABSPkg n r v p ns

instance Show ABSPkg where
    show = pkgNameWithVersionDemand

instance Eq ABSPkg where
  a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

repoOf :: ABSPkg -> String
repoOf (ABSPkg _ r _ _ _) = r

-- | File system root for the synchronised ABS tree.
absBasePath :: FilePath
absBasePath = "/var/abs"

pkgbuildPath :: String -> String -> FilePath
pkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

absSync :: Aura ()
absSync = void $ A.shellCmd "abs" []

-- | Construct a ABSPkg for a string.
absPkg :: String -> Aura ABSPkg
absPkg pkgName = do
  pkgs <- absSearchLookup pkgName
  case (find (\a -> pkgNameOf a == pkgName) pkgs) of
    Just a -> return a
    Nothing -> failure $ "No matching packages for " ++ pkgName

-- | Search for packages matching the given pattern.
absSearchLookup :: String -> Aura [ABSPkg]
absSearchLookup pattern = do
  pkgs <- findPkg pattern
  mapM (\a -> liftIO (readFileUTF8 $ a </> "PKGBUILD") >>= parsePkgBuild a) pkgs

-- | Parse a PKGBUILD into ABSPkg if possible. Fails if:
--   - Unable to extract repo name from the location
--   - Unable to extract a specific key from the PKGBUILD.
parsePkgBuild :: String -- ^ Package location on disk
              -> String -- ^ PKGBUILD contents as string
              -> Aura ABSPkg
parsePkgBuild pkgloc pkgbuild =
  let repo' = case reverse $ split '/' pkgloc of
        _ : a : _ -> return a
        _ -> failure $ "Unable to extract repository name: " ++ pkgloc
      ns' = namespace pkgloc pkgbuild
      getVal ns key = case value ns key of
        a : _ -> return a
        [] -> failure $ "Unable to extract value for key " ++ key
  in do
    repo <- repo'
    ns <- ns'
    name <- getVal ns "pkgname"
    version <- MustBe `liftM` getVal ns "pkgver"
    return $ ABSPkg name repo version pkgbuild ns 

-- | Find a matching list of packages given a name. This only matches
-- on the name of the package.
-- Returns a list of potential paths to packages (the package directory).
findPkg :: String -> Aura [String]
findPkg pattern =
  let isDownDir = (flip notElem) [".", ".."]
      liftFilter f a = do
        a' <- a
        return $ filter f a'
  in liftIO $ do
    entries <- (liftM $ fmap (absBasePath </>))
      $ liftFilter isDownDir
      $ getDirectoryContents absBasePath
    repos <- filterM doesDirectoryExist entries
    entries' <- liftM concat $ mapM (\repo -> (liftM $ fmap (repo </>))
      $ liftFilter isDownDir
      $ getDirectoryContents repo) repos
    packages <- filterM doesDirectoryExist entries'
    return
      $ filter (\pkg -> pkg =~ pattern) packages

filterABSPkgs :: PkgFilter
filterABSPkgs = filterRepoPkgs