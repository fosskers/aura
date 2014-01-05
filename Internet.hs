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
    ( urlContents
    , urlEncodeVars
    , saveUrlContents ) where

import qualified Data.ByteString as B (hPutStr)

import System.FilePath (splitFileName, (</>))
import Network.Curl    (curlGetString_, CurlBuffer)
import Network.HTTP    (urlEncodeVars)
import System.IO       (hClose, openFile, IOMode(WriteMode))

---

urlContents :: CurlBuffer ty => String -> IO ty
urlContents url = snd `fmap` curlGetString_ url []

saveUrlContents :: FilePath -> String -> IO FilePath
saveUrlContents path url = do
  handle  <- openFile filePath WriteMode
  content <- urlContents url
  B.hPutStr handle content >> hClose handle >> return filePath
    where filePath = path </> file
          (_,file) = splitFileName url
