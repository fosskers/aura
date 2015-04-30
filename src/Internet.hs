-- A library that serves as an abstraction for making HTTP requests.

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
    , saveUrlContents ) where

import           Control.Lens
import qualified Data.ByteString.Lazy as L
import           Network.Wreq
import           System.FilePath (splitFileName, (</>))
import           System.IO (hClose, openFile, IOMode(WriteMode))

---

urlContents :: String -> IO (Maybe L.ByteString)
urlContents url = (^? responseBody) <$> get url

saveUrlContents :: FilePath -> String -> IO (Maybe FilePath)
saveUrlContents fpath url = do
  contents <- urlContents url
  case contents of
    Nothing -> return Nothing
    Just c  -> do
      handle <- openFile filePath WriteMode
      L.hPutStr handle c >> hClose handle >> return (Just filePath)
          where filePath = fpath </> file
                (_,file) = splitFileName url
