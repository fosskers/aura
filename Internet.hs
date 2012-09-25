-- A library that serves as an abstraction for dealing with the internet.
-- At the moment, I'm cheating and using `curl`.

module Internet where

-- System Libraries
import System.FilePath (splitFileName, (</>))
import System.IO
import Network.Curl
import qualified Data.ByteString as B

-- Custom Libraries
import Shell

type Url = String

doesUrlExist :: Url -> IO Bool
doesUrlExist url = do
  (head,_) <- curlHead url []
  return $ head /= "HTTP/1.1 404 Not Found"

getUrlContents :: Url -> IO String
getUrlContents url = do
  (_,cont) <- curlGetString url []
  return cont

curlGetByteString :: URLString -> [CurlOption] -> IO (CurlCode,B.ByteString)
curlGetByteString = curlGetString_

saveUrlContents :: FilePath -> Url -> IO FilePath
saveUrlContents path url = do
  h <- openFile filePath WriteMode
  (_,cont) <- curlGetByteString url []
  B.hPutStr h cont >> hClose h >> return filePath
    where filePath = path </> file
          (_,file) = splitFileName url
