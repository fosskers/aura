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
    , absBasePath
    , filterABSPkgs
    , nameAndRepo
    , pkgsInTree
    , repoOf
    , singleSync
    , treeSearch ) where

import qualified Data.Set as Se

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath  ((</>), takeBaseName)
import Text.Regex.PCRE  ((=~))
import Control.Monad    (filterM, void, unless)
import Data.Maybe       (fromJust)

import Aura.Packages.Repository (filterRepoPkgs)
import Aura.Settings.Base       (absTreeOf, langOf)
import Aura.Conflicts           (buildableConflicts)
import Aura.Pacman              (pacmanOutput)
import Aura.Utils               (optionalPrompt)
import Aura.Monad.Aura
import Aura.Languages
import Aura.Bash
import Aura.Core

import Utilities (readFileUTF8, whenM, split)
import Shell     (ls', ls'')

import qualified Aura.Shell as A  (quietShellCmd, shellCmd)
import qualified Shell      as Sh (quietShellCmd)

---

---------------
-- ABS Packages
---------------
data ABSPkg = ABSPkg String String VersionDemand Pkgbuild Namespace

instance Package ABSPkg where
  pkgNameOf (ABSPkg n _ _ _ _) = n
  versionOf (ABSPkg _ _ v _ _) = v
  conflict = buildableConflicts
  package pkg = do
      repo     <- repository name
      pkgbuild <- liftIO $ readFileUTF8 (pkgbuildPath repo name)
      ABSPkg name repo ver pkgbuild `fmap` namespace name pkgbuild
      where (name,ver) = parseNameAndVersionDemand pkg

instance Buildable ABSPkg where
  pkgbuildOf  (ABSPkg _ _ _ p _)  = p
  namespaceOf (ABSPkg _ _ _ _ ns) = ns
  source p fp = do
      let loc = absBasePath </> repoOf p </> pkgNameOf p
      Sh.quietShellCmd "cp" ["-R",loc,fp]
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
type ABSTree = [(String,Se.Set String)]

absBasePath :: FilePath
absBasePath = "/var/abs"

pkgbuildPath :: String -> String -> FilePath
pkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

pkgsInTree :: [String] -> Aura [String]
pkgsInTree pkgs = absTreeOf `fmap` ask >>= \t -> return (filter (inTree t) pkgs)

inTree :: ABSTree -> String -> Bool
inTree tree p = case repository' tree p of
                  Nothing -> False
                  Just _  -> True

inTree' :: String -> IO Bool
inTree' p = doesFileExist $ absBasePath </> p

-- | The repository a package _should_ belong to.
-- Fails if the package is not in any repository.
repository :: String -> Aura String
repository p = absTreeOf `fmap` ask >>= \tree ->
  if inTree tree p
     then return . fromJust . repository' tree $ p
     else repository'' p

-- | The repository a package belongs to.
repository' :: ABSTree -> String -> Maybe String
repository' [] _ = Nothing
repository' ((r,ps):rs) p | Se.member p ps = Just r
                          | otherwise     = repository' rs p

repository'' :: String -> Aura String
repository'' p = do
  fullName <- nameAndRepo p
  case fullName of
    Nothing -> langOf `fmap` ask >>= failure . repository_1 p
    Just fn -> do
      present <- liftIO (inTree' fn)
      unless present $ singleSync fn
      return . head . split '/' $ fn

nameAndRepo :: String -> Aura (Maybe String)
nameAndRepo p = do
  i <- pacmanOutput ["-Si",p]
  case i of
    "" -> return Nothing
    _  -> do
      let pat = "Repository[ ]+: "
          (_,_,repo) = (head $ lines i) =~ pat :: (String,String,String)
      return . Just $ repo </> p

-- | All repos with all their packages in the local tree.
absTree :: IO ABSTree
absTree = do
  repos <- ls'' absBasePath >>= filterM doesDirectoryExist
  mapM (\r -> ls' r >>= \ps -> return (takeBaseName r, Se.fromList ps)) repos

-- | All packages in the local ABS tree in the form: "repo/package"
flatABSTree :: ABSTree -> [String]
flatABSTree = concatMap fold
    where fold (r,ps) = Se.foldr (\p acc -> (r </> p) : acc) [] ps

-- | All packages in the local ABS tree which match a given pattern.
treeSearch :: String -> Aura [ABSPkg]
treeSearch pattern = absTreeOf `fmap` ask >>= \tree -> do
  let matches = concatMap (fold . snd) tree
      fold    = Se.foldr (\p acc -> if p =~ pattern then p : acc else acc) []
  mapM package matches

-- Make this react to `-x` as well? Wouldn't be hard.
-- It would just be a matter of switching between `shellCmd`
-- and `quietShellCmd`.
-- Should this tell the user how many packages they'll be syncing?
-- | Sync only the parts of the ABS tree which already exists on the system.
absSync :: Aura ()
absSync = whenM (optionalPrompt absSync_1) $ do
  notify absSync_2
  (flatABSTree . absTreeOf) `fmap` ask >>= A.shellCmd "abs"

-- | Must be in the form `repo/pkgname`
singleSync :: String -> Aura ()
singleSync p = notify (singleSync_1 p) >> void (A.quietShellCmd "abs" [p])

filterABSPkgs :: PkgFilter
filterABSPkgs = filterRepoPkgs
