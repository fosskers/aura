{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module    : Linux.Arch.Aur
-- Copyright : (c) Colin Woodbury, 2014 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Access package metadata from the Arch Linux User Repository.

module Linux.Arch.Aur
  ( -- * Types
    AurInfo(..)
    -- * Queries
  , info, search
    -- * Re-exports
    -- | These are __Servant__ types which you could want to manipulate without
    -- directly depending on (and importing) servant.
  , ClientError(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

---

data RPCResp = RPCResp
  { _version     :: Int
  , _type        :: Text
  , _resultCount :: Int
  , _results     :: [AurInfo] } deriving (Show)

instance FromJSON RPCResp where
  parseJSON (Object v) = RPCResp
    <$> v .: "version"
    <*> v .: "type"
    <*> v .: "resultcount"
    <*> v .: "results"

  parseJSON _ = mzero

-- | All relevant information about an AUR package.
data AurInfo = AurInfo
  { aurIdOf          :: Int
  , aurNameOf        :: Text
  , pkgBaseIdOf      :: Int
  , pkgBaseOf        :: Text
  , aurVersionOf     :: Text
  , aurDescriptionOf :: Maybe Text
  , urlOf            :: Maybe Text
  , aurVotesOf       :: Int
  , popularityOf     :: Float
  , dateObsoleteOf   :: Maybe Int
  , aurMaintainerOf  :: Maybe Text
  , submissionDateOf :: Int
  , modifiedDateOf   :: Int
  , urlPathOf        :: Maybe Text
  , dependsOf        :: [Text]
  , makeDepsOf       :: [Text]
  , optDepsOf        :: [Text]
  , conflictsOf      :: [Text]
  , providesOf       :: [Text]
  , licenseOf        :: [Text]
  , keywordsOf       :: [Text] } deriving (Eq,Show)

instance FromJSON AurInfo where
    parseJSON (Object v) = AurInfo
      <$> v .:  "ID"
      <*> v .:  "Name"
      <*> v .:  "PackageBaseID"
      <*> v .:  "PackageBase"
      <*> v .:  "Version"
      <*> v .:? "Description"
      <*> v .:? "URL"
      <*> v .:  "NumVotes"
      <*> v .:  "Popularity"
      <*> (v .: "OutOfDate"  <|> pure Nothing)
      <*> (v .: "Maintainer" <|> pure Nothing)
      <*> v .:  "FirstSubmitted"
      <*> v .:  "LastModified"
      <*> v .:? "URLPath"
      <*> v .:? "Depends"     .!= []
      <*> v .:? "MakeDepends" .!= []
      <*> v .:? "OptDepends"  .!= []
      <*> v .:? "Conflicts"   .!= []
      <*> v .:? "Provides"    .!= []
      <*> v .:? "License"     .!= []
      <*> v .:? "Keywords"    .!= []

    parseJSON _ = mzero

instance ToJSON AurInfo where
  toJSON ai = object
    [ "ID" .= aurIdOf ai
    , "Name" .= aurNameOf ai
    , "PackageBaseID" .= pkgBaseIdOf ai
    , "PackageBase" .= pkgBaseOf ai
    , "Version" .= aurVersionOf ai
    , "Description" .= aurDescriptionOf ai
    , "URL" .= urlOf ai
    , "NumVotes" .= aurVotesOf ai
    , "Popularity" .= popularityOf ai
    , "OutOfDate" .= dateObsoleteOf ai
    , "Maintainer" .= aurMaintainerOf ai
    , "FirstSubmitted" .= submissionDateOf ai
    , "LastModified" .= modifiedDateOf ai
    , "URLPath" .= urlPathOf ai
    , "Depends" .= dependsOf ai
    , "MakeDepends" .= makeDepsOf ai
    , "OptDepends" .= optDepsOf ai
    , "Conflicts" .= conflictsOf ai
    , "Provides" .= providesOf ai
    , "License" .= licenseOf ai
    , "Keywords" .= keywordsOf ai ]

---

type Info = "rpc" :> QueryParam "v" Text
           :> QueryParam "type" Text
           :> QueryParams "arg[]" Text
           :> Get '[JSON] RPCResp

type Search = "rpc" :> QueryParam "v" Text
           :> QueryParam "type" Text
           :> QueryParam "arg" Text
           :> Get '[JSON] RPCResp

type API = Info :<|> Search

api :: Proxy API
api = Proxy

url :: BaseUrl
url = BaseUrl Https "aur.archlinux.org" 443 ""

-- | Make a call to the AUR RPC. Assumes version 5 of the API.
rpcI :: Maybe Text -> Maybe Text -> [Text] -> ClientM RPCResp
rpcS :: Maybe Text -> Maybe Text -> Maybe Text -> ClientM RPCResp
rpcI :<|> rpcS = client api

-- | Perform an @info@ call on one or more package names.
-- Will fail with a `Nothing` if there was a connection/decoding error.
info :: Manager -> [Text] -> IO (Either ClientError [AurInfo])
info m ps = unwrap m $ rpcI (Just "5") (Just "info") ps

-- | Perform a @search@ call on a package name or description text.
-- Will fail with a `Nothing` if there was a connection/decoding error.
search :: Manager -> Text -> IO (Either ClientError [AurInfo])
search m p = unwrap m $ rpcS (Just "5") (Just "search") (Just p)

unwrap :: Manager -> ClientM RPCResp -> IO (Either ClientError [AurInfo])
unwrap m r = fmap _results <$> runClientM r (ClientEnv m url Nothing)
