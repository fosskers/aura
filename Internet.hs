-- A library that serves as an abstraction for dealing with the internet.

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

module Internet
    ( toURL
    , fromURL
    , addParam
    , getUrlContents
    , saveUrlContents ) where

-- System Libraries
import qualified Network.URL as U (exportURL, importURL, add_param, URL)
import qualified Data.ByteString as B (ByteString, hPutStr)
import System.IO (hClose, openFile, IOMode(WriteMode))
import System.FilePath (splitFileName, (</>))
import Control.Monad (liftM)
import Network.Curl ( curlGetString
                    , curlGetString_
                    , URLString
                    , CurlOption
                    , CurlCode )

--------------------------------
-- NETWORK.URL ABSTRACTION LAYER
--------------------------------
type URL = U.URL

fromURL :: URL -> String
fromURL = U.exportURL

toURL :: String -> Maybe URL
toURL = U.importURL

addParam :: URL -> (String,String) -> URL
addParam = U.add_param

-------------
-- CONNECTION
-------------
getUrlContents :: String -> IO String
getUrlContents url = snd `liftM` curlGetString url []

curlGetByteString :: URLString -> [CurlOption] -> IO (CurlCode,B.ByteString)
curlGetByteString = curlGetString_

saveUrlContents :: FilePath -> String -> IO FilePath
saveUrlContents path url = do
  h <- openFile filePath WriteMode
  (_,cont) <- curlGetByteString url []
  B.hPutStr h cont >> hClose h >> return filePath
    where filePath = path </> file
          (_,file) = splitFileName url
