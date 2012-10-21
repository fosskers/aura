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

module Internet where

-- System Libraries
import qualified Data.ByteString as B (ByteString, hPutStr)
import System.IO (hClose, openFile, IOMode(WriteMode))
import System.FilePath (splitFileName, (</>))
import Control.Monad (liftM)
import Network.Curl ( curlHead
                    , curlGetString
                    , curlGetString_
                    , URLString
                    , CurlOption
                    , CurlCode )

type Url = String

-- Arbitrary...?
error404 :: String
error404 = "HTTP/1.1 404 Not Found"

doesUrlExist :: Url -> IO Bool
doesUrlExist url = (no404 . fst) `liftM` curlHead url []
    where no404 = (/=) error404

getUrlContents :: Url -> IO String
getUrlContents url = snd `liftM` curlGetString url []

curlGetByteString :: URLString -> [CurlOption] -> IO (CurlCode,B.ByteString)
curlGetByteString = curlGetString_

saveUrlContents :: FilePath -> Url -> IO FilePath
saveUrlContents path url = do
  h <- openFile filePath WriteMode
  (_,cont) <- curlGetByteString url []
  B.hPutStr h cont >> hClose h >> return filePath
    where filePath = path </> file
          (_,file) = splitFileName url
