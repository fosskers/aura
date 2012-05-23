-- Utility functions that don't fit in a particular library.

module Utilities where

-- System Libraries
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import Distribution.Simple.Utils (withTempDirectory)
import Control.Concurrent (threadDelay)
import Distribution.Verbosity (silent)
import System.Exit (ExitCode(..))
import System.IO (stdout, hFlush)
import Data.List (dropWhileEnd)
import Text.Regex.Posix ((=~))

type Pattern = (String,String)

-- COlOUR THIS!
putStrLnA :: String -> IO ()
putStrLnA s = putStrA $ s ++ "\n"

putStrA :: String -> IO ()
putStrA s = putStr $ "aura >> " ++ s

-- Like break, but kills the element that triggered the break.
hardBreak :: (a -> Bool) -> [a] -> ([a],[a])
hardBreak _ [] = ([],[])
hardBreak p xs = (firstHalf, secondHalf')
    where firstHalf   = takeWhile (not . p) xs
          secondHalf  = dropWhile (not . p) xs
          secondHalf' = if null secondHalf then [] else tail secondHalf

lStrip :: String -> String
lStrip xs = dropWhile (== ' ') xs

rStrip :: String -> String
rStrip xs = dropWhileEnd (== ' ') xs

tripleSnd :: (a,b,c) -> b
tripleSnd (a,b,c) = b

{-
didProcessSucceed :: FilePath -> [String] -> String -> IO Bool
didProcessSucceed cmd args stdin = do
  (exitStatus,_,_) <- readProcessWithExitCode cmd args stdin
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False
-}

-- Replaces a (p)attern with a (t)arget in a line if possible.
replaceByPatt :: [Pattern] -> String -> String
replaceByPatt [] line = line
replaceByPatt ((p,t):ps) line | p == m    = replaceByPatt ps (b ++ t ++ a)
                              | otherwise = line
                         where (b,m,a) = line =~ p :: (String,String,String)

withTempDir :: FilePath -> IO a -> IO a
withTempDir name action = do
  withTempDirectory silent "." name $ \dir -> do
    originalDirectory <- getCurrentDirectory
    setCurrentDirectory dir
    result <- action
    setCurrentDirectory originalDirectory
    return result

didProcessSucceed :: ExitCode -> Bool
didProcessSucceed ExitSuccess = True
didProcessSucceed _           = False

timedMessage :: Int -> [String] -> IO ()
timedMessage delay msgs = mapM_ printMessage msgs
    where printMessage msg = putStr msg >> hFlush stdout >> threadDelay delay

-- I'd like a less hacky way to do this.
-- THIS DOESN'T WORK
terminalWidth :: IO Int
terminalWidth = do
  heightAndWidth <- readProcess "stty" ["size"] ""
  return . read . last . words $ heightAndWidth