-- A library that serves as an abstraction for dealing with the internet.
-- At the moment, I'm cheating and using `curl`.

module Internet where

-- System Libraries
import System.Process (readProcess, readProcessWithExitCode)
import System.FilePath (splitFileName, (</>))

-- Custom Libraries
import Utilities

type Url = String

doesUrlExist :: Url -> IO Bool
doesUrlExist url = do
  (exitStatus,_,_) <- readProcessWithExitCode "curl" ["-f","--head",url] ""
  return $ didProcessSucceed exitStatus

getUrlContents :: Url -> IO String
getUrlContents url = readProcess "curl" ["-L","--fail","--silent",url] ""

saveUrlContents :: FilePath -> Url -> IO FilePath
saveUrlContents path url = readProcess "curl" args "" >> return filePath
    where args     = [url,"-L","--fail","--silent","--output",filePath]
          filePath = path </> file
          (_,file) = splitFileName url
