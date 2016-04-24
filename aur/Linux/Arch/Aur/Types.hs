{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Linux.Arch.Aur.Types
-- Copyright : (c) Colin Woodbury, 2014, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>

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
                       , aurDescriptionOf :: Text
                       , urlOf            :: Text
                       , aurVotesOf       :: Int
                       , popularityOf     :: Float
                       , dateObsoleteOf   :: Maybe Int
                       , aurMaintainerOf  :: Maybe Text
                       , submissionDateOf :: Int
                       , modifiedDateOf   :: Int
                       , urlPathOf        :: Text
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
                           v .:  "Description"        <*>
                           v .:  "URL"                <*>
                           v .:  "NumVotes"           <*>
                           v .:  "Popularity"         <*>
                           (v .: "OutOfDate"  <|> pure Nothing) <*>
                           (v .: "Maintainer" <|> pure Nothing) <*>
                           v .:  "FirstSubmitted"     <*>
                           v .:  "LastModified"       <*>
                           v .:  "URLPath"            <*>
                           v .:? "Depends"     .!= [] <*>
                           v .:? "MakeDepends" .!= [] <*>
                           v .:? "OptDepends"  .!= [] <*>
                           v .:? "Conflicts"   .!= [] <*>
                           v .:? "Provides"    .!= [] <*>
                           v .:? "License"     .!= [] <*>
                           v .:? "Keywords"    .!= []

    parseJSON _ = mzero
