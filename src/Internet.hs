-- A library that serves as an abstraction for dealing with the internet.

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

module Internet
    ( unpack
    , urlContents
    , urlEncodeVars
    , saveUrlContents ) where

import qualified Data.ByteString.Lazy as L

import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Exception    (catch)
import System.FilePath      (splitFileName, (</>))
import Network.HTTP         (urlEncodeVars)
import System.IO            (hClose, openFile, IOMode(WriteMode))
import Network.HTTP.Conduit

---

urlContents :: String -> IO L.ByteString
urlContents url = do
  req <- parseUrl url
  let req2 = req { responseTimeout = Nothing}
  catch (fmap responseBody . withManager . httpLbs $ req2) handleException

handleException :: HttpException -> IO L.ByteString
handleException _ = return (pack "")

saveUrlContents :: FilePath -> String -> IO FilePath
saveUrlContents fpath url = do
  handle  <- openFile filePath WriteMode
  content <- urlContents url
  L.hPutStr handle content >> hClose handle >> return filePath
    where filePath = fpath </> file
          (_,file) = splitFileName url
