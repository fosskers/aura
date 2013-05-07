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
    , absSearch
    , absSync
    , cleanABSTree
    , displayPkgbuild
    , displayPkgDeps ) where

import System.Directory (removeDirectoryRecursive, createDirectory)
import Text.Regex.PCRE  ((=~))

import qualified Aura.Install as I

import Aura.Pkgbuild.Base (trueVersion)
import Aura.Pacman        (pacman)
import Aura.Settings.Base
import Aura.Dependencies
import Aura.Packages.ABS
import Aura.Packages.Repository
import Aura.Colour.Text
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils
import Aura.Bash
import Aura.Core

import Utilities (whenM)

---

-- | All repo dependencies will be installed via pacman.
defaultHandle :: [String] -> BuildHandle
defaultHandle pacOpts =
    BH { pkgLabel = "ABS"
       , initialPF = filterABSPkgs
       , mainPF    = \_ -> return []
       , subPF     = filterRepoPkgs
       , subBuild  = \ps -> pacman (["-S","--asdeps"] ++ pacOpts ++ map pkgNameOf ps) }

-- | All repo dependencies will be built manually.
manualHandle :: [String] -> BuildHandle
manualHandle _ = BH { pkgLabel = "ABS"
                    , initialPF = filterABSPkgs
                    , mainPF    = filterABSPkgs
                    , subPF     = \_  -> return []
                    , subBuild  = \_  -> return () }

-- | Install packages, managing dependencies.
-- We force the types on some polymorphic functions here.
install :: [String] -> [String] -> Aura ()
install pacOpts pkgs = buildABSDeps `fmap` ask >>= \manual -> do
  let handle = if manual then manualHandle else defaultHandle
  I.install b c (handle pacOpts) pacOpts pkgs
    where b = package  :: String -> Aura ABSPkg
          c = conflict :: Settings -> RepoPkg -> Maybe ErrMsg

-- | Get info about the specified package (-i)
absInfo :: [String] -> Aura ()
absInfo pkgs = packages pkgs >>= mapM_ displayAbsPkgInfo

-- | Search ABS for any packages matching the given patterns (-s)
absSearch :: [String] -> Aura ()
absSearch pat = treeSearch pat' >>= mapM_ (liftIO . putStrLn . renderSearch pat')
    where pat' = unwords pat

cleanABSTree :: Aura ()
cleanABSTree = whenM (optionalPrompt cleanABSTree_1) $ do
  warn cleanABSTree_2
  liftIO $ removeDirectoryRecursive absBasePath
  liftIO $ createDirectory absBasePath

displayPkgbuild :: [String] -> Aura ()
displayPkgbuild pkgs =
    (packages pkgs :: Aura [ABSPkg]) >>= mapM_ (liftIO . putStrLn . pkgbuildOf)

displayPkgDeps :: [String] -> Aura ()
displayPkgDeps []   = return ()
displayPkgDeps pkgs = do
  deps <- packages pkgs >>= mapM (depCheck $ defaultHandle [])
  let (subs,mains,_) = groupPkgs deps :: ([RepoPkg],[ABSPkg],[String])
  I.reportPkgsToInstall (defaultHandle []) subs mains ([] :: [ABSPkg])

displayAbsPkgInfo :: ABSPkg -> Aura ()
displayAbsPkgInfo pkg = ask >>= liftIO . putStrLn . renderPkgInfo pkg

renderPkgInfo :: ABSPkg -> Settings -> String
renderPkgInfo pkg ss = entrify ss fields entries
  where ns      = namespaceOf pkg
        fields  = map bForeground . absInfoFields . langOf $ ss
        entries = [ magenta $ repoOf pkg
                  , bForeground $ pkgNameOf pkg
                  , trueVersion ns
                  , unwords $ value ns "depends"
                  , unwords $ value ns "makedepends"
                  , concat  $ value ns "pkgdesc" ]

renderSearch :: String -> ABSPkg -> String
renderSearch pat pkg = repo ++ "/" ++ n ++ " " ++ v ++ " \n    " ++ d
    where c cl cs = case cs =~ ("(?i)" ++ pat) of
                      (b,m,a) -> cl b ++ bCyan m ++ cl a
          repo = magenta $ repoOf pkg
          ns   = namespaceOf pkg
          n    = c bForeground $ pkgNameOf pkg
          v    = green $ trueVersion ns
          d    = head $ value ns "pkgdesc"
