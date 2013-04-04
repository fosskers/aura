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
module Aura.ABS (
   absInfoLookup
  ,absSearchLookup
  ,PkgInfo(..)
  )
where

import           Control.Monad    (filterM, forM, liftM, join)
import           Data.List        (intercalate)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, mapMaybe)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath
import           Text.Regex.PCRE  ((=~))

import           Aura.Bash
import           Aura.Core
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Utils       (scoldAndFail)

import           Bash.Base
import           Utilities        (split, readFileUTF8)

-- Stuff --

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String VersionDemand Pkgbuild Namespace

instance Show ABSPkg where
  show = pkgNameWithVersionDemand

instance Eq ABSPkg where
  a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

instance Package ABSPkg where
  pkgNameOf (ABSPkg n _ _ _) = n
  versionOf (ABSPkg _ v _ _) = v

instance SourcePackage ABSPkg where
  pkgbuildOf (ABSPkg _ _ p _) = p
  namespaceOf (ABSPkg _ _ _ n) = n
  getSource a fp = undefined

-- | File system root for the synchronised ABS tree.
absBasePath :: FilePath
absBasePath = "/var/abs"

-- | Get PKGBUILD location from a package name (just appends /PKGBUILD)
pkgBuildFile :: FilePath -> FilePath
pkgBuildFile pkgName = absBasePath </> pkgName </> "PKGBUILD"

data PkgInfo = PkgInfo {
                       -- | Repository (core/extra etc.)
                       repositoryOf    :: String
                       -- | Name of the package (not including repo)
                       , nameOf        :: String
                       -- | Latest available version
                       , latestVerOf   :: String
                       -- | Directory containing the package source (in ABS tree)
                       , locationOf    :: String
                       -- | Package description
                       , descriptionOf :: String
                       } deriving (Eq,Show)

-- | Get info about the named package from the exact package name.
absInfoLookup :: String -> Aura PkgInfo
absInfoLookup pkgName = do
  pkgbuild <- liftIO $ readFileUTF8 $ pkgBuildFile pkgName
  parsePkgBuild pkgName pkgbuild

-- | Search for packages matching the given pattern.
absSearchLookup :: String -> Aura [PkgInfo]
absSearchLookup pattern = do
  pkg <- findPkg pattern
  mapM absInfoLookup pkg

-- | Parse a PKGBUILD into PkgInfo if possible
parsePkgBuild :: String -> String -> Aura PkgInfo
parsePkgBuild pkgloc pkgbuild =
  let repo' = case reverse $ split '/' pkgloc of
        _ : a : _ -> return a
        _ -> failure $ "Unable to extract repository name: " ++ pkgloc
      ns' = namespace pkgloc pkgbuild
      getVal ns key = case value ns key of
        a : _ -> return a
        [] -> failure $ "Unable to extract value for key " ++ key
  in do
    ns <- ns'
    name <- getVal ns "pkgname"
    version <- getVal ns "pkgver"
    desc <- getVal ns "pkgdesc"
    repo <- repo'
    return $ PkgInfo repo name version pkgloc desc

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
