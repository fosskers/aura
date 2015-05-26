{-# LANGUAGE FlexibleContexts #-}
-- Handles all ABS related functions.

{-

Copyright 2012, 2013, 2014
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
    ( absLookup
    , absRepo
    , absDepsRepo
    , ABSTree
    , absBasePath
    , absTree
    , pkgRepo
    , absPkgbuild
    , syncRepo
    , absSync
    , singleSync
    , PkgInfo(..)
    , absInfoLookup
    , absSearchLookup
    ) where

import           Control.Monad
import           Data.List          (find)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import qualified Data.Traversable   as Traversable
import           System.Directory   (doesFileExist, doesDirectoryExist)
import           System.FilePath    ((</>), takeBaseName)
import           Text.Regex.PCRE    ((=~))

import           Aura.Bash
import           Aura.Core
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman        (pacmanOutput)
import           Aura.Pkgbuild.Base
import           Aura.Settings.Base
import qualified Aura.Shell         as A (quietShellCmd, shellCmd)
import           Aura.Utils         (optionalPrompt)

import           Utilities          (readFileUTF8, whenM)
import           Shell              (ls', ls'')
import qualified Shell              as Sh (quietShellCmd)

---

absLookup :: String -> Aura (Maybe Buildable)
absLookup name = syncRepo name >>= maybe (return Nothing) makeSynced
  where makeSynced repo = do
            whenM (not <$> synced repo name) $ singleSync repo name
            found <- synced repo name
            if found
                then Just <$> makeBuildable repo name
                else return Nothing  -- split package, probably

absRepo :: Repository
absRepo = Repository $ absLookup >=> Traversable.mapM packageBuildable

absDepsRepo :: Aura Repository
absDepsRepo = asks (getRepo . buildABSDeps)
  where getRepo manual = if manual then absRepo else pacmanRepo

makeBuildable :: String -> String -> Aura Buildable
makeBuildable repo name = do
  pb <- absPkgbuild repo name
  return Buildable { baseNameOf   = name
                   , pkgbuildOf   = pb
                   , isExplicit   = False
                   , buildScripts = \fp -> Just <$> copyTo repo name fp }

copyTo :: String -> String -> FilePath -> IO FilePath
copyTo repo name fp = do
    void $ Sh.quietShellCmd "cp" ["-R",loc,fp]
    return $ fp </> name
        where loc = absBasePath </> repo </> name

-------
-- WORK
-------
newtype ABSTree = ABSTree [(String,Set String)]

absBasePath :: FilePath
absBasePath = "/var/abs"

-- | All repos with all their packages in the local tree.
absTree :: Aura ABSTree
absTree = liftIO $ do
    repos <- ls'' absBasePath >>= filterM doesDirectoryExist
    ABSTree <$> mapM populate repos
  where
    populate repo = do
        ps <- ls' repo
        return (takeBaseName repo, Set.fromList ps)

pkgRepo :: ABSTree -> String -> Maybe String
pkgRepo (ABSTree repos) p = fst <$> find containsPkg repos
  where
    containsPkg (_, ps) = Set.member p ps

-- | All packages in the local ABS tree in the form: "repo/package"
flatABSTree :: ABSTree -> [String]
flatABSTree (ABSTree repos) = concatMap flat repos
  where
    flat (r, ps) = map (r </>) $ Set.toList ps

absPkgbuildPath :: String -> String -> FilePath
absPkgbuildPath repo pkg = absBasePath </> repo </> pkg </> "PKGBUILD"

absPkgbuild :: String -> String -> Aura Pkgbuild
absPkgbuild repo pkg = liftIO $ readFileUTF8 (absPkgbuildPath repo pkg)

syncRepo :: String -> Aura (Maybe String)
syncRepo p = do
  i <- pacmanOutput ["-Si",p]
  case i of
    "" -> return Nothing
    _  -> do
      let pat = "Repository[ ]+: "
          (_,_,repo) = head (lines i) =~ pat :: (String,String,String)
      return $ Just repo

synced :: String -> String -> Aura Bool
synced repo pkg = liftIO . doesFileExist $ absPkgbuildPath repo pkg

-- Make this react to `-x` as well? Wouldn't be hard.
-- It would just be a matter of switching between `shellCmd`
-- and `quietShellCmd`.
-- Should this tell the user how many packages they'll be syncing?
-- | Sync only the parts of the ABS tree which already exists on the system.
absSync :: Aura ()
absSync = whenM (optionalPrompt absSync_1) $ do
    notify absSync_2
    ps <- flatABSTree <$> absTree
    A.shellCmd "abs" ps

singleSync :: String -> String -> Aura ()
singleSync repo name = do
    notify $ singleSync_1 p
    void $ A.quietShellCmd "abs" [p]
  where
    p = repo </> name

data PkgInfo = PkgInfo
    { nameOf        :: String
    , repoOf        :: String
    , trueVersionOf :: String
    , dependsOf     :: [String]
    , makeDependsOf :: [String]
    , descriptionOf :: String
    }

pkgInfo :: String -> String -> Aura PkgInfo
pkgInfo repo name = do
    pb <- absPkgbuild repo name
    ns <- namespace name pb
    return PkgInfo
        { nameOf        = name
        , repoOf        = repo
        , trueVersionOf = trueVersion ns
        , dependsOf     = value ns "depends"
        , makeDependsOf = value ns "makedepends"
        , descriptionOf = concat $ value ns "pkgdesc"
        }

absInfoLookup :: ABSTree -> String -> Aura (Maybe PkgInfo)
absInfoLookup tree name =
    Traversable.mapM (\repo -> pkgInfo repo name) $ pkgRepo tree name

-- | All packages in the local ABS tree which match a given pattern.
absSearchLookup :: ABSTree -> String -> Aura [PkgInfo]
absSearchLookup (ABSTree tree) pattern = mapM (uncurry pkgInfo) matches
  where
    matches       = concatMap match tree
    match (r, ps) = map (\p -> (r, p)) . filter (=~ pattern) $ Set.toList ps
