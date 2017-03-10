{-

Copyright 2017 Jiehong Ma <email@majiehong.com>

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

module SrcInfoType (SrcInfo(..), srcInfoData) where

import Data.Text
import Data.Int
import Data.Eq
import Data.Maybe
import BasicPrelude


data SrcInfo = SrcInfo { name        :: Maybe Text
                       , version     :: Maybe Text
                       , release     :: Maybe Int
                       , epoch       :: Maybe Text
                       , arch        :: Maybe [Text]
                       , licenses    :: Maybe [Text]
                       , makeDepends :: Maybe [Text]
                       , provides    :: Maybe [Text]
                       , conflicts   :: Maybe [Text]
                       , sources     :: Maybe [Text]
                       , md5sums     :: Maybe [Text]
                       , sha1sums    :: Maybe [Text]
                       , sha224sums  :: Maybe [Text]
                       , sha256sums  :: Maybe [Text]
                       , sha384sums  :: Maybe [Text]
                       , sha512sums  :: Maybe [Text]
                       } deriving (Eq, Show)

srcInfoData :: SrcInfo
srcInfoData = SrcInfo { name        = Nothing
                      , version     = Nothing
                      , release     = Nothing
                      , epoch       = Nothing
                      , arch        = Nothing
                      , licenses    = Nothing
                      , makeDepends = Nothing
                      , provides    = Nothing
                      , conflicts   = Nothing
                      , sources     = Nothing
                      , md5sums     = Nothing
                      , sha1sums    = Nothing
                      , sha224sums  = Nothing
                      , sha256sums  = Nothing
                      , sha384sums  = Nothing
                      , sha512sums  = Nothing
                      }