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

module Aura.AurConnection
    ( aurInfoLookup
    , aurSearchLookup
    , getTrueVerViaPkgbuild
    , downloadPkgbuild
    , downloadSource
    , Pkgbuild
    , PkgInfo(..) ) where

import System.FilePath ((</>))
import Control.Monad   (liftM)
import Data.Maybe      (fromJust)
import Text.JSON

import Aura.Utils (scoldAndFail)
import Aura.Monad.Aura
import Aura.Languages

import Bash.Base

import Internet

-----------------------
-- AUR API URL CREATION
-----------------------
data RPCType = PkgSearch | MultiInfo | MSearch deriving (Eq)

aurPkgUrl :: Int -> String
aurPkgUrl n = "https://aur.archlinux.org/packages.php?ID=" ++ show n

rpcBaseUrl :: String
rpcBaseUrl = "https://aur.archlinux.org/rpc.php"

-- the `fromJust` should never fail.
makeRPCUrl :: RPCType -> [String] -> String
makeRPCUrl t ps = fromURL . addParams . fromJust . toURL $ rpcBaseUrl
    where addParams u      = addParam (addP u t) $ rpcType t
          addP u MultiInfo = foldl (\u' p -> addParam u' ("arg[]",p)) u ps
          addP u _         = addParam u ("arg",unwords ps)

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
  infoJSON <- liftIO . getUrlContents . makeRPCUrl t $ items
  case resultToEither $ parseInfoJSON infoJSON of
    Left _     -> scoldAndFail getAURPkgInfoMsg1
    Right info -> return info

parseInfoJSON :: String -> Result [PkgInfo]
parseInfoJSON json = decode json >>= apiFailCheck >>= forgePkgInfo
    where forgePkgInfo j = valFromObj "results" j >>= mapM makePkgInfo

apiFailCheck :: JSObject JSValue -> Result (JSObject JSValue)
apiFailCheck json = do
  isError <- (== "error") `liftM` valFromObj "type" json
  if isError then Error "AUR API lookup failed." else Ok json

-- Upgrade to AUR 2.0 changed several return types to Ints,
-- but Text.JSON parses them as Rationals.
makePkgInfo :: JSObject JSValue -> Result PkgInfo
makePkgInfo pkgJSON = do
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
type Pkgbuild = String

aurLink :: String
aurLink = "https://aur.archlinux.org/packages/"

getPkgBaseUrl :: String -> String
getPkgBaseUrl pkg = aurLink </> take 2 pkg </> pkg

getPkgbuildUrl :: String -> String
getPkgbuildUrl pkg = getPkgBaseUrl pkg </> "PKGBUILD"                     

downloadPkgbuild :: String -> Aura Pkgbuild
downloadPkgbuild = liftIO . getUrlContents . getPkgbuildUrl

getTrueVerViaPkgbuild :: Namespace -> String
getTrueVerViaPkgbuild ns = pkgver ++ "-" ++ pkgrel
    where pkgver  = head . fromJust . getVar ns $ "pkgver"
          pkgrel  = head . fromJust . getVar ns $ "pkgrel"

------------------
-- SOURCE TARBALLS
------------------
getTarballUrl :: String -> String
getTarballUrl pkg = getPkgBaseUrl pkg </> pkg ++ ".tar.gz"

downloadSource :: FilePath -> String -> IO FilePath
downloadSource path = saveUrlContents path . getTarballUrl 
