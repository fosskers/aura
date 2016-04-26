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

import           BasicPrelude hiding (liftIO)

import           Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.ICU as Re

import           Aura.Colour.Text
import           Aura.Core
import           Aura.Install (InstallOptions(..))
import qualified Aura.Install as I
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Packages.ABS
import           Aura.Settings.Base
import           Aura.Utils

import           Shelly hiding (whenM,liftIO)
import           Utilities (whenM)

---

installOptions :: Aura InstallOptions
installOptions = do
    depsRepo <- absDepsRepo
    pure InstallOptions
        { label         = "ABS"
        , installLookup = absLookup
        , repository    = depsRepo
        }

install :: [T.Text] -> [T.Text] -> Aura ()
install pacOpts ps = do
    opts <- installOptions
    I.install opts pacOpts ps

-- | Sync given packages to the local ABS Tree.
addToTree :: [T.Text] -> Aura ()
addToTree = traverse_ $ \p ->
    syncRepo p >>= maybe (pure ()) (\repo -> singleSync repo p)

-- | Get info about the specified package (-i)
absInfo :: [T.Text] -> Aura ()
absInfo ps = do
    tree <- absTree
    infos <- catMaybes <$> traverse (absInfoLookup tree) ps
    traverse_ displayAbsPkgInfo infos

-- | Search ABS for any packages matching the given patterns (-s)
absSearch :: [T.Text] -> Aura ()
absSearch pat = do
    tree <- absTree
    infos <- absSearchLookup tree pat'
    traverse_ (liftIO . IO.putStrLn . renderSearch pat') infos
  where
    pat' = T.unwords pat

cleanABSTree :: Aura ()
cleanABSTree = whenM (optionalPrompt cleanABSTree_1) $ do
  warn cleanABSTree_2
  liftShelly $ rm_rf absBasePath
  liftShelly $ mkdir absBasePath

displayPkgbuild :: [T.Text] -> Aura ()
displayPkgbuild ps = I.displayPkgbuild displayPkgbuild' ps
  where displayPkgbuild' :: [T.Text] -> Aura [Maybe Pkgbuild]
        displayPkgbuild' ps' = do
          tree <- absTree
          forM ps' $ \p -> case pkgRepo tree p of
            Nothing   -> pure Nothing
            Just repo -> Just <$> absPkgbuild repo p

displayPkgDeps :: [T.Text] -> Aura ()
displayPkgDeps ps = do
    opts <- installOptions
    I.displayPkgDeps opts ps

displayAbsPkgInfo :: PkgInfo -> Aura ()
displayAbsPkgInfo pkg = ask >>= liftIO . IO.putStrLn . renderPkgInfo pkg

renderPkgInfo :: PkgInfo -> Settings -> T.Text
renderPkgInfo pkg ss = entrify ss fields (($ pkg) <$> entries)
  where fields  = fmap bForeground . absInfoFields . langOf $ ss
        entries = [ magenta . repoOf
                  , bForeground . nameOf
                  , trueVersionOf
                  , T.unwords . dependsOf
                  , T.unwords . makeDependsOf
                  , descriptionOf
                  ]

renderSearch :: T.Text -> PkgInfo -> T.Text
renderSearch pat pkg = repo <> "/" <> n <> " " <> v <> " \n    " <> d
    where c cl cs = fromMaybe cs ((\match -> cl (Re.span match)
                                            <> bCyan (fromMaybe ""
                                                      (Re.group 0 match))
                                            <> cl (fromMaybe ""
                                                   (Re.suffix 0 match)))
                    <$> Re.find (Re.regex [] ("(?i)"<> pat)) cs)
          repo = magenta $ repoOf pkg
          n    = c bForeground $ nameOf pkg
          v    = trueVersionOf pkg
          d    = descriptionOf pkg
