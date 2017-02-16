-- An interface to Data.Time

{-

Copyright 2012, 2013, 2014, 2015 Colin Woodbury <colingw@gmail.com>

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
    ( dotFormat
    , localTime
    , Time ) where

import Data.List (intercalate)
import Data.Time hiding (months)
import Text.Printf (printf)

---

data Time = Time { yearOf   :: Integer
                 , monthOf  :: Int
                 , dayOf    :: Int
                 , hourOf   :: Int
                 , minuteOf :: Int
                 , secondOf :: Int }
            deriving (Eq, Show, Read)

months :: [String]
months = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"
         , "Oct", "Nov", "Dec" ]

localTime :: IO Time
localTime = toTime . zonedTimeToLocalTime <$> getZonedTime

toTime :: LocalTime -> Time
toTime t = Time { yearOf   = ye
                , monthOf  = mo
                , dayOf    = da
                , hourOf   = todHour $ localTimeOfDay t
                , minuteOf = todMin $ localTimeOfDay t
                , secondOf = round . todSec $ localTimeOfDay t }
    where (ye, mo, da) = toGregorian $ localDay t

dotFormat :: Time -> String
dotFormat t = intercalate "." items
    where items = [ show $ yearOf t
                  , printf "%02d(%s)" (monthOf t) (months !! (monthOf t - 1))
                  , printf "%02d" (dayOf t)
                  , printf "%02d" (hourOf t)
                  , printf "%02d" (minuteOf t)
                  , printf "%02d" (secondOf t) ]
