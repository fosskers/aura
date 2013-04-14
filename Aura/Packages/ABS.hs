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
module Aura.Packages.ABS
--    ( absSearchLookup
    ( absSync
    , filterABSPkgs
    , repoOf
--    , findPkg
    , ABSPkg ) where

import System.Directory (doesDirectoryExist)
import System.FilePath  ((</>), takeBaseName)
import Text.Regex.PCRE  ((=~))
import Control.Monad    (filterM, liftM)
import Data.Maybe       (mapMaybe)
import Data.List        (find)

import Aura.Packages.Repository (filterRepoPkgs)
import Aura.Monad.Aura
import Aura.Core
import Aura.Bash

import Utilities (readFileUTF8, split)
import Shell     (ls'')

import qualified Aura.Shell as A (shellCmd)
import qualified Shell      as S (quietShellCmd)

---

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String String VersionDemand Pkgbuild Namespace

instance Package ABSPkg where
  pkgNameOf (ABSPkg n _ _ _ _) = n
  versionOf (ABSPkg _ _ v _ _) = v
  package pkgName = undefined
      
      
{-}
      pkgs <- absSearchLookup pkgName
      case (find (\a -> pkgNameOf a == pkgName) pkgs) of
        Just a -> return a
        Nothing -> failure $ "No matching packages for " ++ pkgName
-}

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

-------
-- WORK
-------
absBasePath :: FilePath
absBasePath = "/var/abs"

pkgbuildPath :: String -> String -> FilePath
pkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

repository :: String -> IO (Maybe String)
repository pkg = undefined

-- | Yield full filepaths to all packages in the ABS tree.
-- TODO: MAKE THIS RETURN [(String,Set String)]
-- Where each tuple contains a repo name and all the packages in it.
absTree :: IO [FilePath]
absTree = do
  repos <- ls'' absBasePath >>= filterM doesDirectoryExist
  concat `liftM` mapM ls'' repos

-- | All packages in the ABS tree that matched a pattern.
absLookup :: String -> Aura [ABSPkg]
absLookup pattern = do
  pkgs <- filter (\pkg -> takeBaseName pkg =~ pattern) `liftM` liftIO absTree
  mapM (\a -> liftIO (readFileUTF8 $ a </> "PKGBUILD") >>= parsePkgBuild a) pkgs

-- Make this react to `-x` as well? Wouldn't be hard.
-- It would just be a matter of switching between `shellCmd`
-- and `quietShellCmd`.
-- | Sync only the parts of the ABS tree which already exists on the system.
absSync :: Aura ()
absSync = mapMaybe repoAndName `liftM` liftIO absTree >>= A.shellCmd "abs"
    where repoAndName pkg = case reverse $ split '/' pkg of
            n:r:_ -> Just $ r </> n
            _     -> Nothing

singleSync :: String -> Aura ()
singleSync = A.shellCmd "abs" . (: [])

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

filterABSPkgs :: PkgFilter
filterABSPkgs = filterRepoPkgs
