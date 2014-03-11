-- A Parser for version numbers.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.Utils.Numbers where

import Text.ParserCombinators.Parsec
import Control.Applicative (pure, (<*),(*>),(<*>),(<$>),(<$))
import Data.Char           (digitToInt)

---

data Version = Version { unitsOf    :: [Unit]
                       , revisionOf :: Maybe Int }  -- The number after `-`.
               deriving (Eq,Show,Ord)

data Unit = IUnit Int | SUnit String deriving (Eq,Show,Ord)

parseVersion :: String -> Either ParseError Version
parseVersion = parse version ""

version :: Parser Version
version = Version <$> units <*> optionMaybe revision

units :: Parser [Unit]
units = concat <$> (many1 (iunit <|> sunit) `sepBy` char '.')

iunit :: Parser Unit
iunit = IUnit . crunchInt <$> many1 digit

sunit :: Parser Unit
sunit = SUnit <$> many1 letter

revision :: Parser Int
revision = char '-' *> pure crunchInt <*> many1 digit

crunchInt :: String -> Int
crunchInt = foldl (\acc i -> acc * 10 + digitToInt i) 0
