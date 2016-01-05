{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

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

import           BasicPrelude  hiding    (FilePath, liftIO, (</>))

import           Data.Foldable      (fold)
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           Shelly   hiding    ((</>),find,liftIO)
import           Filesystem.Path.CurrentOS
import qualified Data.Text.ICU      as Re

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

import           Utilities          (readFileUTF8, exists)
import           Aura.Shell         (lsT', ls'')

---

absLookup :: T.Text -> Aura (Maybe Buildable)
absLookup name = syncRepo name >>= maybe (pure Nothing) makeSynced
  where makeSynced :: T.Text -> Aura (Maybe Buildable)
        makeSynced repo = do
            whenM (not <$> synced repo name) $ singleSync repo name
            found <- synced repo name
            if found
                then Just <$> makeBuildable repo name
                else pure Nothing  -- split package, probably

absRepo :: Repository
absRepo = Repository $ absLookup >=> traverse packageBuildable

absDepsRepo :: Aura Repository
absDepsRepo = asks (getRepo . buildABSDeps)
  where getRepo manual = if manual then absRepo else pacmanRepo

makeBuildable :: T.Text -> T.Text -> Aura Buildable
makeBuildable repo name = do
  pb <- absPkgbuild repo name
  pure Buildable { baseNameOf   = name
                 , pkgbuildOf   = pb
                 , isExplicit   = False
                 , buildScripts = \fp -> Just <$> copyTo repo name fp }

copyTo :: T.Text -> T.Text -> FilePath -> Aura FilePath
copyTo repo name fp = do
    _ <- A.quietShellCmd "cp" ["-R",toTextIgnore loc, toTextIgnore fp]
    pure $ fp </> fromText name
        where loc = absBasePath </> fromText repo </> fromText name

-------
-- WORK
-------
newtype ABSTree = ABSTree [(T.Text, Set T.Text)]

absBasePath :: FilePath
absBasePath = "/var/abs"

-- | All repos with all their packages in the local tree.
absTree :: Aura ABSTree
absTree = liftShelly $ do
    repos <- ls'' absBasePath >>= filterM test_d
    ABSTree <$> traverse populate repos
  where
    populate repo = do
        ps <- lsT' repo
        pure (toTextIgnore $ basename repo, Set.fromList ps)

pkgRepo :: ABSTree -> T.Text -> Maybe T.Text
pkgRepo (ABSTree repos) p = fst <$> find containsPkg repos
  where
    containsPkg (_, ps) = Set.member p ps

-- | All packages in the local ABS tree in the form: "repo/package"
flatABSTree :: ABSTree -> [T.Text]
flatABSTree (ABSTree repos) = foldMap flat repos
  where
    flat (r, ps) = (\b -> toTextIgnore (fromText r </> fromText b)) <$> Set.toList ps

absPkgbuildPath :: T.Text -> T.Text -> FilePath
absPkgbuildPath repo pkg = absBasePath </> fromText repo </> fromText pkg </> "PKGBUILD"

absPkgbuild :: T.Text -> T.Text -> Aura Pkgbuild
absPkgbuild repo pkg = liftIO $ readFileUTF8 (absPkgbuildPath repo pkg)

syncRepo :: T.Text -> Aura (Maybe T.Text)
syncRepo p = do
  i <- pacmanOutput ["-Si", p]
  case i of
    "" -> pure Nothing
    _  -> do
      let pat = "Repository[ ]+: "
          match = Re.find (Re.regex [] pat) $ head (T.lines i)
      pure (Re.suffix 0 =<< match)

synced :: T.Text -> T.Text -> Aura Bool
synced repo pkg = liftShelly . exists $ absPkgbuildPath repo pkg

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

singleSync :: T.Text -> T.Text -> Aura ()
singleSync repo name = do
    notify $ singleSync_1 p
    void $ A.quietShellCmd "abs" [p]
  where
    p = toTextIgnore (fromText repo </> fromText name)

data PkgInfo = PkgInfo
    { nameOf        :: T.Text
    , repoOf        :: T.Text
    , trueVersionOf :: T.Text
    , dependsOf     :: [T.Text]
    , makeDependsOf :: [T.Text]
    , descriptionOf :: T.Text
    }

pkgInfo :: T.Text -> T.Text -> Aura PkgInfo
pkgInfo repo name = do
    pb <- absPkgbuild repo name
    ns <- namespace name pb
    pure PkgInfo
        { nameOf        = name
        , repoOf        = repo
        , trueVersionOf = trueVersion ns
        , dependsOf     = value ns "depends"
        , makeDependsOf = value ns "makedepends"
        , descriptionOf = fold $ value ns "pkgdesc"
        }

absInfoLookup :: ABSTree -> T.Text -> Aura (Maybe PkgInfo)
absInfoLookup tree name =
    traverse (\repo -> pkgInfo repo name) $ pkgRepo tree name

-- | All packages in the local ABS tree which match a given pattern.
absSearchLookup :: ABSTree -> T.Text -> Aura [PkgInfo]
absSearchLookup (ABSTree tree) pattern = traverse (uncurry pkgInfo) matches
  where
    matches       = foldMap match tree
    match (r, ps) = fmap (\p -> (r, p)) . filter (isJust . Re.find (Re.regex [] pattern))  $ Set.toList ps
