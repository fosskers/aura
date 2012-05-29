-- A library that serves as an abstraction for dealing with the internet.
-- At the moment, I'm cheating and using `curl`.

module Internet where

-- System Libraries
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode)

-- Custom Libraries
import Utilities

type Url = String

getUrlContents :: Url -> IO String
getUrlContents url = readProcess "curl" ["-f",url] ""

doesUrlExist :: Url -> IO Bool
doesUrlExist url = do
  (exitStatus,_,_) <- readProcessWithExitCode "curl" ["-f","--head",url] ""
  return $ didProcessSucceed exitStatus