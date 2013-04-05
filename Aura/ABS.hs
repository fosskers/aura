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
  ,renderPkgInfo
  ,PkgInfo
  )
where

import           Control.Monad      (filterM, liftM)
import           System.Directory   (doesDirectoryExist, getDirectoryContents)
import           System.FilePath
import           Text.Regex.PCRE    ((=~))

import qualified Aura.Bash          as B
import           Aura.Core
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Settings.Base
import           Aura.Utils         (entrify)

import           Bash.Base
import           Shell              (shellCmd)
import           Utilities          (readFileUTF8, split)

-- Stuff --

---------------
-- ABS Packages
---------------

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
                       , latestVerOf   :: VersionDemand
                       -- | Directory containing the package source (in ABS tree)
                       , locationOf    :: String
                       -- | Package description
                       , descriptionOf :: String
                       -- | PKGBUILD read into a string.
                       , pkgbuild      :: String
                       -- | Namespace (key/value) pairs.
                       , namespace     :: Namespace
                       } deriving (Show)

instance Package PkgInfo where
  pkgNameOf = nameOf
  versionOf = latestVerOf

instance Eq PkgInfo where
  a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

instance SourcePackage PkgInfo where
  pkgbuildOf = pkgbuild
  namespaceOf = namespace
  getSource a fp = do
    let loc = locationOf a
    shellCmd "cp" ["-R", loc, fp]
    return $ fp </> (takeBaseName loc)
  parsePkgbuild = parseLocalPkgBuild

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

-- | Format a PkgInfo into a string
renderPkgInfo :: Settings -> PkgInfo -> String
renderPkgInfo ss info = entrify ss fields entries
  where fields  = map (pcWhite ss) . absInfoFields . langOf $ ss
        entries = [ pcMagenta ss $ repositoryOf info
                  , pcWhite ss $ nameOf info
                  , show . latestVerOf $ info
                  , locationOf info
                  , descriptionOf info ]

-- | Parse a PKGBUILD into PkgInfo if possible. Fails if:
--   - Unable to extract repo name from the location
--   - Unable to extract a specific key from the PKGBUILD.
parsePkgBuild :: String -- ^ Package location on disk
              -> String -- ^ PKGBUILD contents as string
              -> Aura PkgInfo
parsePkgBuild pkgloc pkgbuild =
  let repo' = case reverse $ split '/' pkgloc of
        _ : a : _ -> return a
        _ -> failure $ "Unable to extract repository name: " ++ pkgloc
  in do
    repo <- repo'
    parsePkgBuild' repo pkgloc pkgbuild

{- | Parse a PKGBUILD for a 'local' package - e.g. one not residing
in an ABS repository location. This will be true for any packages
that are being built.
-}
parseLocalPkgBuild :: String -- ^ PKGBuild location (not that important)
                   -> String -- ^ PKGBUILD contents as string
                   -> Aura PkgInfo
parseLocalPkgBuild pkgloc pkgbuild = 
  parsePkgBuild' "local" pkgloc pkgbuild

parsePkgBuild' :: String -- ^ Repository name
               -> String -- ^ Pkgbuild location on disk.
               -> String -- ^ PKGBUILD contents
               -> Aura PkgInfo
parsePkgBuild' repo pkgloc pkgbuild = 
  let ns' = B.namespace pkgloc pkgbuild
      getVal ns key = case B.value ns key of
        a : _ -> return a
        [] -> failure $ "Unable to extract value for key " ++ key
  in do
    ns <- ns'
    name <- getVal ns "pkgname"
    version <- MustBe `liftM` getVal ns "pkgver"
    desc <- getVal ns "pkgdesc"
    return $ PkgInfo repo name version pkgloc desc pkgbuild ns               

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
