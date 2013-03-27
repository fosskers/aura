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

module Aura.ABS (
   absInfoLookup
  ,absSearchLookup
  ,PkgInfo(..)
  )
where

import           Control.Monad    (filterM, forM, liftM)
import           Data.List        (intercalate)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, mapMaybe)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath
import           Text.Regex.PCRE  ((=~))

import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Utils       (scoldAndFail)

import           Bash.Base
import           Utilities        (split)

-- Stuff --

-- | PKGBUILD file read into a string.
type PkgBuild = String

-- | File system root for the synchronised ABS tree.
absBasePath :: FilePath
absBasePath = "/var/abs"

-- | Get PKGBUILD location from a package name (just appends /PKGBUILD)
pkgBuildFile :: FilePath -> FilePath
pkgBuildFile pkgName = absBasePath </> pkgName </> "PKGBUILD"

data PkgInfo = PkgInfo {
                       -- | Repository
                       repositoryOf    :: String
                       -- | Name of the package (not including repo)
                       , nameOf          :: String
                       -- | Latest available version
                       , latestVerOf   :: String
                       -- | Location of
                       , locationOf :: String
                       -- | Package description
                       , descriptionOf :: String
                       } deriving (Eq,Show)

-- | Get info about the named package from the exact package name.
absInfoLookup :: String -> Aura PkgInfo
absInfoLookup pkgName = liftIO $ do
  pkgbuild <- readFile $ pkgBuildFile pkgName
  case parsePkgBuild pkgName pkgbuild of
    Just pi -> return pi
    Nothing -> fail $ "No info for package " ++ pkgName

absSearchLookup :: String -> Aura [PkgInfo]
absSearchLookup pattern = do
  pkg <- findPkg pattern
  mapM absInfoLookup pkg

-- | Parse a PKGBUILD into PkgInfo if possible
parsePkgBuild :: String -> String -> Maybe PkgInfo
parsePkgBuild pkgloc pkgbuild =
  let l = lines pkgbuild
      props = mapMaybe (\line -> case split '=' line of
        a : b : _ -> Just (a,b)
        _ -> Nothing
        ) l
      propMap = M.fromList props
      repo' = case reverse $ split '/' pkgloc of
        _ : a : _ -> Just a
        _ -> Nothing
  in do
    name <- M.lookup "pkgname" propMap
    version <- M.lookup "pkgver"propMap
    desc <- M.lookup "pkgdesc" propMap
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
