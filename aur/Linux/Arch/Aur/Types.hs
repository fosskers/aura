{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Linux.Arch.Aur.Types
-- Copyright : (c) Colin Woodbury, 2014 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>

module Linux.Arch.Aur.Types
    ( RPCResp(..)
    , AurInfo(..)
    ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Text

---

data RPCResp = RPCResp { _version :: Int
                       , _type :: Text
                       , _resultCount :: Int
                       , _results :: [AurInfo] } deriving (Show)

instance FromJSON RPCResp where
  parseJSON (Object v) = RPCResp <$>
    v .: "version" <*>
    v .: "type" <*>
    v .: "resultcount" <*>
    v .: "results"

  parseJSON _ = mzero

data AurInfo = AurInfo { aurIdOf          :: Int
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
    parseJSON (Object v) = AurInfo                    <$>
                           v .:  "ID"                 <*>
                           v .:  "Name"               <*>
                           v .:  "PackageBaseID"      <*>
                           v .:  "PackageBase"        <*>
                           v .:  "Version"            <*>
                           v .:? "Description"        <*>
                           v .:? "URL"                <*>
                           v .:  "NumVotes"           <*>
                           v .:  "Popularity"         <*>
                           (v .: "OutOfDate"  <|> pure Nothing) <*>
                           (v .: "Maintainer" <|> pure Nothing) <*>
                           v .:  "FirstSubmitted"     <*>
                           v .:  "LastModified"       <*>
                           v .:? "URLPath"            <*>
                           v .:? "Depends"     .!= [] <*>
                           v .:? "MakeDepends" .!= [] <*>
                           v .:? "OptDepends"  .!= [] <*>
                           v .:? "Conflicts"   .!= [] <*>
                           v .:? "Provides"    .!= [] <*>
                           v .:? "License"     .!= [] <*>
                           v .:? "Keywords"    .!= []

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
