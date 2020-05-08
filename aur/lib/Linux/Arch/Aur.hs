{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module    : Linux.Arch.Aur
-- Copyright : (c) Colin Woodbury, 2014 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Access package metadata from the Arch Linux User Repository.

module Linux.Arch.Aur
  ( -- * Types
    AurInfo(..)
  , AurError(..)
    -- * Queries
  , info, search
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

---

data RPCResp = RPCResp
  { _version     :: Int
  , _type        :: Text
  , _resultCount :: Int
  , _results     :: [AurInfo] }
  deriving (Show)

instance FromJSON RPCResp where
  parseJSON = withObject "RPCResp" $ \v -> RPCResp
    <$> v .: "version"
    <*> v .: "type"
    <*> v .: "resultcount"
    <*> v .: "results"

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
    parseJSON = withObject "AurInfo" $ \v -> AurInfo
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

data AurError = NotFound ByteString | BadJSON | OtherAurError ByteString
  deriving stock (Eq, Ord, Show)

-- | Perform an @info@ call on one or more package names.
-- Will fail with a `Left` if there was a connection/decoding error.
info :: Manager -> [Text] -> IO (Either AurError [AurInfo])
info m ps = work m url
  where
    url = "https://aur.archlinux.org/rpc?v=5&type=info&" <> as
    as = T.unpack . T.intercalate "&" $ map ("arg%5B%5D=" <>) ps

-- | Perform a @search@ call on a package name or description text.
-- Will fail with a `Left` if there was a connection/decoding error.
search :: Manager -> Text -> IO (Either AurError [AurInfo])
search m p = work m url
  where
    url = "https://aur.archlinux.org/rpc?v=5&type=search&arg=" <> T.unpack p

work :: Manager -> String -> IO (Either AurError [AurInfo])
work m url = do
  req <- parseRequest url
  res <- httpLbs req m
  case responseStatus res of
    Status 200 _ -> pure . maybe (Left BadJSON) (Right . _results) . decode' $ responseBody res
    Status 404 e -> pure . Left $ NotFound e
    Status _ e   -> pure . Left $ OtherAurError e
