{-# LANGUAGE OverloadedStrings #-}

module Arel.Versions
    ( Version(..)
    , version ) where

import Data.Char (digitToInt)
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.Text

import Arel.Util

---

data Version = Version { unitsOf    :: [Unit]
                       , revisionOf :: Maybe Int }  -- The number after `-`.
               deriving (Eq,Show,Read,Ord)

data Unit = IUnit Int | SUnit Text deriving (Eq,Show,Read,Ord)

version :: Text -> Maybe Version
version = eitherToMaybe . parse versionNumber ""

versionNumber :: Parser Version
versionNumber = Version <$> units <*> optionMaybe revision

units :: Parser [Unit]
units = concat <$> (many1 (iunit <|> sunit) `sepBy` oneOf ".:_+")

iunit :: Parser Unit
iunit = IUnit . asInt <$> many1 digit

sunit :: Parser Unit
sunit = SUnit . pack <$> many1 letter

revision :: Parser Int
revision = char '-' *> pure asInt <*> many1 digit

asInt :: String -> Int
asInt = foldl (\acc i -> acc * 10 + digitToInt i) 0
