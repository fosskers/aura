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

-- Handles all ABS related functions.

module Aura.Packages.ABS where

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

import Utilities (readFileUTF8, split)

import qualified Aura.Shell as A (quietShellCmd)  -- Aura - Has failure checks
import qualified Shell      as S (quietShellCmd)  -- IO   - Doesn't

---

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String String VersionDemand Pkgbuild Namespace

instance Package ABSPkg where
  pkgNameOf (ABSPkg n _ _ _ _) = n
  versionOf (ABSPkg _ _ v _ _) = v

instance Buildable ABSPkg where
  pkgbuildOf  (ABSPkg _ _ _ p _)  = p
  namespaceOf (ABSPkg _ _ _ _ ns) = ns
  source p fp = do
      let loc = absBasePath </> repoOf p </> pkgNameOf p
      S.quietShellCmd "cp" ["-R",loc,fp]
      return $ fp </> pkgNameOf p
  rewrap (ABSPkg n r v p ns) ns' = ABSPkg n r v p ns'
  buildable pkg = do
      repo <- repository name
      absSync repo name
      pkgbuild <- liftIO . readFile . pkgbuildPath repo $ name
      ABSPkg name repo ver pkgbuild `liftM` namespace name pkgbuild
          where (name,ver) = parseNameAndVersionDemand pkg

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

-- | The repository that a package belongs to.
-- For now, only packages in the three official repos are allowed.
repository :: String -> Aura String
repository p = do
  info <- (head . lines) `liftM` pacmanOutput ["-Si",p]
  let (_,_,repo) = info =~ "Repository[ ]+: " :: (String,String,String)
  if not $ repo =~ "(^core|^community|^extra)"
     then failure $ repo </> p ++ " cannot be synced."
     else return repo

absSync :: String -> String -> Aura ()
absSync repo name = void $ A.quietShellCmd "abs" [repo </> name]

{-}
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
  where fields  = map white . absInfoFields . langOf $ ss
        entries = [ magenta $ repositoryOf info
                  , white $ nameOf info
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
-}