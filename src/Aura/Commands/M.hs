{-# LANGUAGE FlexibleContexts #-}
{- Handles all `-M` operations for building from the ABS.

* On `-M` suboptions in general *
 Note that `-M` interacts with your _local_ copy of the
 Arch Build System Tree. `-i` `-p` or `-s` are thus _local_ searches
 of whatever you have in your /var/abs/
 If a package you're looking for isn't present in the local tree,
 nothing will show up, even if you know it otherwise exists in
 the repositories.

* On `-y` *
 Using `-My` makes an `abs` shell call on all the packages in your
 local tree. It does _not_ sync the entire ABS tree. For that, simply
 use `sudo abs`.

* On Building Packages *
 Using just `-M` to build a package from the ABS tree will attempt
 to build with the PKGBUILD from your local tree. If it doesn't
 exist, a fresh copy will be synced with `abs` and then built.

-}

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

module Aura.Commands.M
    ( install
    , absInfo
    , absSearch
    , absSync
    , addToTree
    , cleanABSTree
    , displayPkgbuild
    , displayPkgDeps ) where

import System.Directory (removeDirectoryRecursive, createDirectory)
import Text.Regex.PCRE  ((=~))
import Control.Monad
import Data.Maybe       (catMaybes)

import           Aura.Install (InstallOptions(..))
import qualified Aura.Install as I

import Aura.Settings.Base
import Aura.Packages.ABS
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Core

import Utilities (whenM)

---

installOptions :: Aura InstallOptions
installOptions = do
    depsRepo <- absDepsRepo
    return InstallOptions
        { label         = "ABS"
        , installLookup = absLookup
        , repository    = depsRepo
        }

install :: [String] -> [String] -> Aura ()
install pacOpts ps = do
    opts <- installOptions
    I.install opts pacOpts ps

-- | Sync given packages to the local ABS Tree.
addToTree :: [String] -> Aura ()
addToTree = mapM_ $ \p ->
    syncRepo p >>= maybe (return ()) (\repo -> singleSync repo p)

-- | Get info about the specified package (-i)
absInfo :: [String] -> Aura ()
absInfo ps = do
    tree <- absTree
    infos <- catMaybes <$> mapM (absInfoLookup tree) ps
    mapM_ displayAbsPkgInfo infos

-- | Search ABS for any packages matching the given patterns (-s)
absSearch :: [String] -> Aura ()
absSearch pat = do
    tree <- absTree
    infos <- absSearchLookup tree pat'
    mapM_ (liftIO . putStrLn . renderSearch pat') infos
  where
    pat' = unwords pat

cleanABSTree :: Aura ()
cleanABSTree = whenM (optionalPrompt cleanABSTree_1) $ do
  warn cleanABSTree_2
  liftIO $ removeDirectoryRecursive absBasePath
  liftIO $ createDirectory absBasePath

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild ps = flip I.displayPkgbuild ps $ \ps' -> do
    tree <- absTree
    forM ps' $ \p -> case pkgRepo tree p of
                       Nothing   -> return Nothing
                       Just repo -> Just <$> absPkgbuild repo p

displayPkgDeps :: [String] -> Aura ()
displayPkgDeps ps = do
    opts <- installOptions
    I.displayPkgDeps opts ps

displayAbsPkgInfo :: PkgInfo -> Aura ()
displayAbsPkgInfo pkg = ask >>= liftIO . putStrLn . renderPkgInfo pkg

renderPkgInfo :: PkgInfo -> Settings -> String
renderPkgInfo pkg ss = entrify ss fields (map ($ pkg) entries)
  where fields  = map bForeground . absInfoFields . langOf $ ss
        entries = [ magenta . repoOf
                  , bForeground . nameOf
                  , trueVersionOf
                  , unwords . dependsOf
                  , unwords . makeDependsOf
                  , descriptionOf
                  ]

renderSearch :: String -> PkgInfo -> String
renderSearch pat pkg = repo ++ "/" ++ n ++ " " ++ v ++ " \n    " ++ d
    where c cl cs = case cs =~ ("(?i)" ++ pat) of
                      (b,m,a) -> cl b ++ bCyan m ++ cl a
          repo = magenta $ repoOf pkg
          n    = c bForeground $ nameOf pkg
          v    = trueVersionOf pkg
          d    = descriptionOf pkg
