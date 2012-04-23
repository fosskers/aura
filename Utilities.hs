-- Utility functions that don't fit in a particular library.

module Utilities where

import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))

didProcessSucceed :: FilePath -> [String] -> String -> IO Bool
didProcessSucceed cmd args stdin = do
  (exitStatus,_,_) <- readProcessWithExitCode cmd args stdin
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f xs = do
  bools <- mapM f xs
  return . map fst . filter (\(x,b) -> b) . zip xs $ bools

-- I'd like a less hacky way to do this.
-- THIS DOESN'T WORK
terminalWidth :: IO Int
terminalWidth = do
  heightAndWidth <- readProcess "stty" ["size"] ""
  return . read . last . words $ heightAndWidth