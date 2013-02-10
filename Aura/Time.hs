-- An interface to System.Time

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

module Aura.Time
    ( getClockTime
    , toUTCTime
    , toSimpleTime
    , dotFormat 
    , SimpleTime ) where

import System.Time (getClockTime, toUTCTime, CalendarTime(..), Month(..))
import Text.Printf (printf)
import Data.Maybe  (fromJust)
import Data.List   (intercalate,elemIndex)

---

data SimpleTime = SimpleTime { yearOf   :: Int
                             , monthOf  :: Month
                             , dayOf    :: Int
                             , hourOf   :: Int
                             , minuteOf :: Int }
                  deriving (Eq,Show,Read)

toSimpleTime :: CalendarTime -> SimpleTime
toSimpleTime c = SimpleTime { yearOf   = ctYear c
                            , monthOf  = ctMonth c
                            , dayOf    = ctDay c
                            , hourOf   = ctHour c
                            , minuteOf = ctMin c }

dotFormat :: SimpleTime -> String
dotFormat t = intercalate "." items
    where items = [ show $ yearOf t
                  , printf "%02d(%s)" (monthNum $ monthOf t)
                    (take 3 . show . monthOf $ t)
                  , printf "%02d" (dayOf t)
                  , printf "%02d" (hourOf t)
                  , printf "%02d" (minuteOf t) ]

monthNum :: Month -> Int
monthNum m = 1 + fromJust (elemIndex m [January ..])
