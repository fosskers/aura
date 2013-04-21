{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- Handles all ABS related functions.

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

module Aura.Packages.ABS
    ( ABSPkg
    , absSync
    , absTree
    , filterABSPkgs
    , repoOf ) where

import qualified Data.Set as S

import System.Directory (doesDirectoryExist)
import System.FilePath  ((</>), takeBaseName)
import Text.Regex.PCRE  ((=~))
import Control.Monad    (filterM, when)
import Data.Maybe       (mapMaybe, fromJust)
import Data.List        (find)

import Aura.Packages.Repository (filterRepoPkgs)
import Aura.Settings.Base       (absTreeOf)
import Aura.Pacman              (pacmanOutput)
import Aura.Utils               (optionalPrompt)
import Aura.Monad.Aura
import Aura.Languages
import Aura.Bash
import Aura.Core

import Utilities (readFileUTF8, split, ifM3)
import Shell     (ls', ls'')

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
  package pkg = do
      repo     <- repository pkg
      pkgbuild <- liftIO $ readFileUTF8 (pkgbuildPath repo name)
      ABSPkg name repo ver pkgbuild `fmap` namespace name pkgbuild
      where (name,ver) = parseNameAndVersionDemand pkg

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
type ABSTree = [(String,S.Set String)]

absBasePath :: FilePath
absBasePath = "/var/abs"

pkgbuildPath :: String -> String -> FilePath
pkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

inTree :: ABSTree -> String -> Bool
inTree tree p = case repository' tree p of
                  Nothing -> False
                  Just r  -> True

-- This is pretty ugly.
-- | The repository a package _should_ belong to.
-- Fails if the package is not in any repository.
repository :: String -> Aura String
repository p = absTreeOf `fmap` ask >>= \tree ->
  if inTree tree p
     then return . fromJust . repository' tree $ p
     else do
       i <- pacmanOutput ["-Si",p]
       case i of
         "" -> failure $ p ++ " is not a package in any repository."
         _  -> do
           let pat = "Repository[ ]+: "
               (_,_,repo) = (head $ lines i) =~ pat :: (String,String,String)
           return repo

-- | The repository a package belongs to.
repository' :: ABSTree -> String -> Maybe String
repository' [] _ = Nothing
repository' ((r,ps):rs) p | S.member p ps = Just r
                          | otherwise     = repository' rs p

-- | All repos with all their packages in the local tree.
absTree :: IO ABSTree
absTree = do
  repos <- ls'' absBasePath >>= filterM doesDirectoryExist
  mapM (\r -> ls' r >>= \ps -> return (takeBaseName r, S.fromList ps)) repos

-- | All packages in the local ABS tree in the form: "repo/package"
flatABSTree :: ABSTree -> [String]
flatABSTree = concatMap fold
    where fold (r,ps) = S.foldr (\p acc -> (r </> p) : acc) [] ps

{-}
-- | All packages in the ABS tree that matched a pattern.
absLookup :: String -> Aura [ABSPkg]
absLookup pattern = do
  pkgs <- filter (\pkg -> takeBaseName pkg =~ pattern) `fmap` liftIO absTree
  mapM (\a -> liftIO (readFileUTF8 $ a </> "PKGBUILD") >>= parsePkgBuild a) pkgs
-}

-- Make this react to `-x` as well? Wouldn't be hard.
-- It would just be a matter of switching between `shellCmd`
-- and `quietShellCmd`.
-- Should this tell the user how many packages they'll be syncing?
-- | Sync only the parts of the ABS tree which already exists on the system.
absSync :: Aura ()
absSync = ifM3 (optionalPrompt absSync_1) $ do
            notify absSync_2
            (flatABSTree . absTreeOf) `fmap` ask >>= A.shellCmd "abs"

singleSync :: String -> Aura ()
singleSync = A.shellCmd "abs" . (: [])

filterABSPkgs :: PkgFilter
filterABSPkgs = filterRepoPkgs
