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
   getABSPkgInfo
  ,findPkg
  )
where

import           Control.Monad    (filterM, liftM, forM)
import           Data.List        (intercalate)
import           Data.Maybe       (fromJust,mapMaybe)
import qualified Data.Map as M
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath
import           Text.Regex.PCRE  ((=~))

import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Utils       (scoldAndFail)

import           Bash.Base
import Utilities (split)

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
                       -- | Name of the package (not including repo)
                       nameOf          :: String
                       -- | Latest available version
                       , latestVerOf   :: String
                       -- | Package description
                       , descriptionOf :: String
                       } deriving (Eq,Show)

-- | Get info about the named package from the exact package name.
getABSPkgInfo :: String -> Aura PkgInfo
getABSPkgInfo pkgName = liftIO $ do
  pkgbuild <- readFile $ pkgBuildFile pkgName
  case parsePkgBuild pkgbuild of
    Just pi -> return pi
    Nothing -> fail $ "No info for package " ++ pkgName

-- | Parse a PKGBUILD into PkgInfo if possible
parsePkgBuild :: String -> Maybe PkgInfo
parsePkgBuild pkgbuild =
  let l = lines pkgbuild
      props = mapMaybe (\line -> case split '=' line of
        a : b : _ -> Just (a,b)
        _ -> Nothing
        ) l
      propMap = M.fromList props
  in do
    name <- M.lookup "pkgname" propMap
    version <- M.lookup "pkgver"propMap 
    desc <- M.lookup "pkgdesc" propMap
    return $ PkgInfo name version desc

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
    repos <- (liftM $ fmap (absBasePath </>))
      $ liftFilter isDownDir 
      $ getDirectoryContents absBasePath
    packages <- mapM (\repo -> (liftM $ fmap (repo </>)) 
      $ liftFilter isDownDir 
      $ getDirectoryContents repo) repos
    return 
      $ filter (\pkg -> pkg =~ pattern) 
      $ (liftM concat) packages