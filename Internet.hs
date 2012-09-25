-- A library that serves as an abstraction for dealing with the internet.

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
