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
    ( filterAURPkgs
    , aurInfoLookup
    , aurSearchLookup
    , downloadPkgbuild
    , sourceTarball
    , PkgInfo(..)
    , AURPkg(..) ) where

import Control.Applicative ((<$>), (<*>), pure)
import System.FilePath     ((</>))
import Data.List           (intercalate)
import Text.JSON

import Aura.Utils (scoldAndFail)
import Aura.Bash  (value, namespace, Namespace)
import Aura.Monad.Aura
import Aura.Languages
import Aura.Core

import Utilities (decompress)
import Internet

---

data AURPkg = AURPkg String VersionDemand Pkgbuild Namespace 

instance Package AURPkg where
  pkgNameOf (AURPkg n _ _ _) = n
  versionOf (AURPkg _ v _ _) = v
  package pkg = do
      pkgbuild <- downloadPkgbuild name
      AURPkg name ver pkgbuild `fmap` namespace name pkgbuild
          where (name,ver) = parseNameAndVersionDemand pkg

instance Buildable AURPkg where
  pkgbuildOf  (AURPkg _ _ p _)  = p
  namespaceOf (AURPkg _ _ _ ns) = ns
  source p fp = sourceTarball fp (pkgNameOf p) >>= decompress
  rewrap (AURPkg n v p _) ns = AURPkg n v p ns

instance Show AURPkg where
    show = pkgNameWithVersionDemand

instance Eq AURPkg where
    a == b = pkgNameWithVersionDemand a == pkgNameWithVersionDemand b

filterAURPkgs :: PkgFilter
filterAURPkgs pkgs = map nameOf `fmap` aurInfoLookup pkgs

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
                       , projectURLOf  :: String
                       , aurURLOf      :: String
                       , licenseOf     :: String
                       , descriptionOf :: String
                       , maintainerOf  :: Maybe String
                       , isOutOfDate   :: Bool
                       , votesOf       :: Int } deriving (Eq,Show)

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
  isError <- (== "error") `fmap` valFromObj "type" json
  if isError then Error "AUR API lookup failed." else Ok json

-- For some reason, if I forego the `maintainer` variable with:
--   pure (resultToMaybe $ valFromObj "Maintainer" pkgJSON)
-- it refuses to compile.
pkgInfo :: JSObject JSValue -> Result PkgInfo
pkgInfo pkgJSON = PkgInfo
                  <$> valFromObj "Name" pkgJSON
                  <*> valFromObj "Version" pkgJSON
                  <*> valFromObj "URL" pkgJSON
                  <*> (aurPkgUrl . fromJSRat) `fmap` valFromObj "ID" pkgJSON
                  <*> valFromObj "License" pkgJSON
                  <*> valFromObj "Description" pkgJSON
                  <*> pure maintainer
                  <*> ((/= 0) . fromJSRat) `fmap` valFromObj "OutOfDate" pkgJSON
                  <*> fromJSRat `fmap` valFromObj "NumVotes" pkgJSON
    where maintainer = resultToMaybe $ valFromObj "Maintainer" pkgJSON

fromJSRat :: JSValue -> Int
fromJSRat (JSRational _ r) = round (fromRational r :: Float)
fromJSRat _                = error "JSValue given was not a JSRational!"

-- | A companion to the provided `resultToEither` function.
resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok a) = Just a
resultToMaybe _      = Nothing

----------------
-- AUR PKGBUILDS
----------------
aurLink :: String
aurLink = "https://aur.archlinux.org/packages/"

pkgBaseUrl :: String -> String
pkgBaseUrl pkg = aurLink </> take 2 pkg </> pkg

pkgbuildUrl :: String -> String
pkgbuildUrl pkg = pkgBaseUrl pkg </> "PKGBUILD"

downloadPkgbuild :: String -> Aura Pkgbuild
downloadPkgbuild = liftIO . urlContents . pkgbuildUrl

------------------
-- SOURCE TARBALLS
------------------
tarballUrl :: String -> String
tarballUrl pkg = pkgBaseUrl pkg </> pkg ++ ".tar.gz"

sourceTarball :: FilePath    -- ^ Where to save the tarball.
              -> String      -- ^ Package name.
              -> IO FilePath -- ^ Saved tarball location.
sourceTarball path = saveUrlContents path . tarballUrl
