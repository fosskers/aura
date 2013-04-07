-- Module for connecting to the AUR servers,
-- downloading PKGBUILDs and source tarballs, and handling them.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

module Aura.Packages.AUR
    ( aurPkg
    , filterAURPkgs
    , aurInfoLookup
    , aurSearchLookup
    , trueVerViaPkgbuild
    , downloadPkgbuild
    , sourceTarball
    , PkgInfo(..)
    , AURPkg(..) ) where

import System.FilePath ((</>))
import Control.Monad   (liftM)
import Data.List       (intercalate)
import Text.JSON

import Aura.Bash
import Aura.Core
import Aura.Monad.Aura
import Aura.Languages
import Aura.Utils (scoldAndFail)

import Utilities (decompress)

import Internet

---

data AURPkg = AURPkg String VersionDemand Pkgbuild Namespace 

instance Package AURPkg where
  pkgNameOf (AURPkg n _ _ _) = n
  versionOf (AURPkg _ v _ _) = v

instance Buildable AURPkg where
  pkgbuildOf  (AURPkg _ _ p _)  = p
  namespaceOf (AURPkg _ _ _ ns) = ns
  source p fp = sourceTarball fp (pkgNameOf p) >>= decompress
  rewrap (AURPkg n v p ns) ns' = AURPkg n v p ns'

instance Show AURPkg where
    show = pkgNameWithVersionDemand

instance Eq AURPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

---------
-- AURPkg
---------
aurPkg :: String -> Aura AURPkg
aurPkg pkg = do
  pkgbuild  <- downloadPkgbuild name
  AURPkg name ver pkgbuild `liftM` namespace name pkgbuild
      where (name,ver) = parseNameAndVersionDemand pkg

filterAURPkgs :: PkgFilter
filterAURPkgs pkgs = map nameOf `liftM` aurInfoLookup pkgs

-----------------------
-- AUR API URL CREATION
-----------------------
data RPCType = PkgSearch | MultiInfo | MSearch deriving (Eq)

aurPkgUrl :: Int -> String
aurPkgUrl n = "https://aur.archlinux.org/packages.php?ID=" ++ show n

rpcBaseUrl :: String
rpcBaseUrl = "https://aur.archlinux.org/rpc.php?"

-- Had to do a bit off a hack, since `urlEncodeVars` wasn't encoding
-- things in the necessary way.
rpcUrl :: RPCType -> [String] -> String
rpcUrl t ps = rpcBaseUrl ++ ps'
    where ps' = intercalate "&" (t' : encodedPs t)
          encodedPs MultiInfo = map (\p -> urlEncodeVars [("arg[]",p)]) ps
          encodedPs _         = [urlEncodeVars [("arg",unwords ps)]]
          t' = urlEncodeVars [rpcType t]

rpcType :: RPCType -> (String,String)
rpcType t = ("type",tname)
    where tname = case t of
                    PkgSearch -> "search"
                    MultiInfo -> "multiinfo"
                    MSearch   -> "msearch"

-------
-- JSON
-------
-- Extend this later as needed.
data PkgInfo = PkgInfo { nameOf        :: String
                       , latestVerOf   :: String
                       , isOutOfDate   :: Bool
                       , projectURLOf  :: String
                       , aurURLOf      :: String
                       , licenseOf     :: String
                       , votesOf       :: Int
                       , descriptionOf :: String } deriving (Eq,Show)

aurSearchLookup :: [String] -> Aura [PkgInfo]
aurSearchLookup regex = getAURPkgInfo regex PkgSearch

aurInfoLookup :: [String] -> Aura [PkgInfo]
aurInfoLookup pkgs = getAURPkgInfo pkgs MultiInfo

getAURPkgInfo :: [String] -> RPCType -> Aura [PkgInfo]
getAURPkgInfo [] _    = return []
getAURPkgInfo items t = do
  infoJSON <- liftIO . urlContents . rpcUrl t $ items
  case resultToEither $ parseInfoJSON infoJSON of
    Left _     -> scoldAndFail getAURPkgInfo_1
    Right info -> return info

parseInfoJSON :: String -> Result [PkgInfo]
parseInfoJSON json = decode json >>= apiFailCheck >>= forgePkgInfo
    where forgePkgInfo j = valFromObj "results" j >>= mapM pkgInfo

apiFailCheck :: JSObject JSValue -> Result (JSObject JSValue)
apiFailCheck json = do
  isError <- (== "error") `liftM` valFromObj "type" json
  if isError then Error "AUR API lookup failed." else Ok json

-- Upgrade to AUR 2.0 changed several return types to Ints,
-- but Text.JSON parses them as Rationals.
pkgInfo :: JSObject JSValue -> Result PkgInfo
pkgInfo pkgJSON = do
  ur <- valFromObj "URL" pkgJSON
  na <- valFromObj "Name" pkgJSON
  ve <- valFromObj "Version" pkgJSON
  li <- valFromObj "License" pkgJSON
  vo <- fromJSRat `liftM` valFromObj "NumVotes" pkgJSON
  de <- valFromObj "Description" pkgJSON
  au <- (aurPkgUrl . fromJSRat) `liftM` valFromObj "ID" pkgJSON
  ou <- ((/= 0) . fromJSRat) `liftM` valFromObj "OutOfDate" pkgJSON
  return $ PkgInfo na ve ou ur au li vo de

fromJSRat :: JSValue -> Int
fromJSRat (JSRational _ r) = round (fromRational r :: Float)
fromJSRat _                = error "JSValue given was not a JSRational!"

------------
-- PKGBUILDS
------------
aurLink :: String
aurLink = "https://aur.archlinux.org/packages/"

pkgBaseUrl :: String -> String
pkgBaseUrl pkg = aurLink </> take 2 pkg </> pkg

pkgbuildUrl :: String -> String
pkgbuildUrl pkg = pkgBaseUrl pkg </> "PKGBUILD"

downloadPkgbuild :: String -> Aura Pkgbuild
downloadPkgbuild = liftIO . urlContents . pkgbuildUrl

trueVerViaPkgbuild :: Namespace -> String
trueVerViaPkgbuild ns = pkgver ++ "-" ++ pkgrel
    where pkgver = head $ value ns "pkgver"
          pkgrel = head $ value ns "pkgrel"

------------------
-- SOURCE TARBALLS
------------------
tarballUrl :: String -> String
tarballUrl pkg = pkgBaseUrl pkg </> pkg ++ ".tar.gz"

sourceTarball :: FilePath -- ^ Where to save the tarball.
  -> String  -- ^ Package name.
  -> IO FilePath -- ^ Saved tarball location.
sourceTarball path = saveUrlContents path . tarballUrl
