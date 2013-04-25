{- Handles all `-M` operations for building from the ABS.

* On `-M` suboptions in general *
 Note that `-M` interacts with your _local_ copy of the
 Arch Build System Tree. `-i` `-p` or `-s` are thus _local_ searches
 of whatever you have in your /var/abs/

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

module Aura.Commands.M
    ( install
    , absInfo
--    , absSearch
    , absSync
    , displayPkgbuild
    , displayPkgDeps ) where

import Text.Regex.PCRE ((=~))
import Data.List       (nub, nubBy)

import qualified Aura.Install as I

import Aura.Pkgbuild.Base (trueVersion)
import Aura.Settings.Base
import Aura.Dependencies
import Aura.Packages.ABS
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Bash
import Aura.Core

---

-- | Install packages, managing dependencies
install :: [String] -> [String] -> Aura ()
install pacOpts pkgs = I.install b filterABSPkgs pacOpts pkgs
    where b = package :: String -> Aura ABSPkg  -- Force the type.

-- | Get info about the specified package (-i)
absInfo :: [String] -> Aura ()
absInfo pkgs = packages pkgs >>= mapM_ displayAbsPkgInfo

-- | Search ABS for any packages matching the given patterns (-s)
{-}
absSearch :: [String] -> Aura ()
absSearch search = do
  q <- mapM lookupWithTerm search
  mapM_ (uncurry displaySearch) $ concat q
  where lookupWithTerm term = absSearchLookup term >>= 
          mapM (\r -> return (term, r))
-}

-- | Display PKGBUILD
displayPkgbuild :: [String] -> Aura ()
displayPkgbuild pkgs =
    (packages pkgs :: Aura [ABSPkg]) >>= mapM_ (liftIO . putStrLn . pkgbuildOf)

-- | Display package dependencies
displayPkgDeps :: [String] -> Aura ()
displayPkgDeps []   = return ()
displayPkgDeps pkgs = do
  deps <- (packages pkgs :: Aura [ABSPkg]) >>= mapM (depCheck filterABSPkgs)
  let (ps,as,_) = foldl groupPkgs ([],[],[]) deps
  I.reportPkgsToInstall (n ps) (nubBy sameName as) []
    where n = nub . map splitName
          sameName a b = pkgNameOf a == pkgNameOf b

----------
-- Helpers
----------
-- | Display ABS package info
displayAbsPkgInfo :: ABSPkg -> Aura ()
displayAbsPkgInfo pkg = ask >>= \ss ->
  liftIO . putStrLn . renderPkgInfo ss $ pkg

-- | Format an ABSPkg into a string
renderPkgInfo :: Settings -> ABSPkg -> String
renderPkgInfo ss pkg = entrify ss fields entries
  where ns      = namespaceOf pkg
        fields  = map bForeground . absInfoFields . langOf $ ss
        entries = [ magenta $ repoOf pkg
                  , bForeground $ pkgNameOf pkg
                  , trueVersion ns
                  , unwords $ value ns "depends"
                  , unwords $ value ns "makedepends"
                  , concat $ value ns "pkgdesc" ]

-- | Display search results. Search term will be highlighted in the results.
displaySearch :: String -> ABSPkg -> Aura ()
displaySearch term pkg = ask >>= \ss ->
  liftIO . putStrLn $ renderSearch ss term pkg

-- | Render an ABSPkg into a search string.
renderSearch :: Settings -> String -> ABSPkg -> String
renderSearch ss r i =
  repo ++ "/" ++ n ++ " " ++ v ++ " \n    " ++ d
    where c cl cs = case cs =~ ("(?i)" ++ r) of
                      (b,m,a) -> cl b ++ bCyan m ++ cl a
          repo = magenta $ repoOf i
          ns = namespaceOf i
          n = c bForeground $ pkgNameOf i
          v = green . tail . show $ versionOf i
          d = case (value ns "pkgdesc") of
            a : _ -> c noColour a
            _ -> red . missingDescription $ langOf ss
