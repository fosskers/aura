-- Handles all `-M` operations for building from the ABS.

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
    , absSearch
    , absSync
    , absSyncLocal
    , displayPkgbuild
    , displayPkgDeps ) where

import Data.List       (nub, nubBy)
import Text.Regex.PCRE ((=~))

import qualified Aura.Install as I

import Aura.Bash
import Aura.Dependencies
import Aura.Packages.ABS
import Aura.Monad.Aura
import Aura.Core
import Aura.Colour.Text
import Aura.Languages
import Aura.Settings.Base
import Aura.Utils

-- | Get info about the specified package (-i)
absInfo :: [String] -> Aura ()
absInfo search = do
  q <- mapM package search
  mapM_ displayAbsPkgInfo q

-- | Search ABS for any packages matching the given patterns (-s)
absSearch :: [String] -> Aura ()
absSearch search = do
  q <- mapM lookupWithTerm search
  mapM_ (uncurry displaySearch) $ concat q
  where lookupWithTerm term = absSearchLookup term >>= 
          mapM (\r -> return (term, r))

-- | Display PKGBUILD
displayPkgbuild :: [String] -> Aura ()
displayPkgbuild pkgNames = do
  pkgs <- mapM absPkg pkgNames
  mapM_ (liftIO . putStrLn . pkgbuildOf) pkgs

-- | Display package dependencies
displayPkgDeps :: [String] -> Aura ()
displayPkgDeps []   = return ()
displayPkgDeps pkgs = do
  info    <- mapM package pkgs :: Aura [ABSPkg]
  allDeps <- mapM (depCheck filterABSPkgs) info
  let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
  I.reportPkgsToInstall (n ps) (nubBy sameName as) []
    where n = nub . map splitName
          sameName a b = pkgNameOf a == pkgNameOf b

-- | Install packages, managing dependencies
install :: [String] -> [String] -> Aura ()
install pacOpts pkgs = I.install b filterABSPkgs pacOpts pkgs
    where b = package :: String -> Aura ABSPkg  -- Force the type.

----------
-- Helpers
----------

-- | Display ABS package info
displayAbsPkgInfo :: ABSPkg -> Aura ()
displayAbsPkgInfo info = ask >>= \ss -> do
  let pkginfo = renderPkgInfo ss info
  liftIO $ putStrLn $ pkginfo ++ "\n"

-- | Format an ABSPkg into a string
renderPkgInfo :: Settings -> ABSPkg -> String
renderPkgInfo ss info = entrify ss fields entries
  where ns = namespaceOf info
        fields  = map white . absInfoFields . langOf $ ss
        description = case (value ns "pkgdesc") of
          a : _ -> a
          _ -> red . missingDescription $ langOf ss
        entries = [ magenta $ repoOf info
                    , white $ pkgNameOf info
                    , show . versionOf $ info
                    , description ]

-- | Display search results. Search term will be highlighted in the results.
displaySearch :: String -- ^ Search term
              -> ABSPkg -- ^ Search result
              -> Aura ()
displaySearch term info = do
  ss <- ask
  let pkginfo = renderSearch ss term info
  liftIO $ putStrLn pkginfo

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
