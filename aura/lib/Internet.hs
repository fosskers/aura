-- A library that serves as an abstraction for making HTTP requests.

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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

module Internet ( urlContents ) where

import           BasePrelude hiding (handle)
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)

---

-- TODO Move to utilities
-- | Assumes the given URL is correctly formatted.
urlContents :: Manager -> String -> IO (Maybe L.ByteString)
urlContents m url = f <$> httpLbs (parseRequest_ url) m
  where f res | statusCode (responseStatus res) == 200 = Just $ responseBody res
              | otherwise = Nothing
