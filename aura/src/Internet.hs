-- A library that serves as an abstraction for making HTTP requests.

{-

Copyright 2012 - 2016 Colin Woodbury <colingw@gmail.com>

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

module Internet
    ( urlContents
    , saveUrlContents ) where

import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Network.URI (unEscapeString)
import           System.FilePath (splitFileName, (</>))
import           System.IO (hClose, openFile, IOMode(WriteMode))

---

-- | Assumes the given URL is correctly formatted.
urlContents :: Manager -> String -> IO (Maybe L.ByteString)
urlContents m url = fmap f $ httpLbs (parseRequest_ url) m
  where f res | statusCode (responseStatus res) == 200 = Just $ responseBody res
              | otherwise = Nothing

-- | Fetch data from some URL, and save it to the filesystem.
saveUrlContents :: Manager -> FilePath -> String -> IO (Maybe FilePath)
saveUrlContents m fpath url = do
  contents <- urlContents m url
  case contents of
    Nothing -> pure Nothing
    Just c  -> do
      handle <- openFile filePath WriteMode
      L.hPutStr handle c *> hClose handle *> pure (Just filePath)
          where filePath = fpath </> file
                (_, file) = splitFileName $ unEscapeString url
