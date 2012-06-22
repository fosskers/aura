-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

-- System Libraries
import System.Process (readProcessWithExitCode, rawSystem)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Regex.Posix ((=~))

-- Custom Libraries
import AuraLanguages
import Utilities 

type Arg = String

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

packageCache :: FilePath
packageCache = "/var/cache/pacman/pkg/"

pacman :: [Arg] -> IO ()
pacman args = rawSystem "pacman" args >> return ()

-- Runs pacman without producing any output.
pacmanQuiet :: [Arg] -> IO (ExitCode,String,String)
pacmanQuiet args = readProcessWithExitCode "pacman" args ""

-- Did a pacman process succeed?
pacmanSuccess :: [Arg] -> IO Bool
pacmanSuccess args = do
  (exitStatus,_,_) <- pacmanQuiet args
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False

pacmanFailure :: [Arg] -> IO Bool
pacmanFailure args = pacmanSuccess args >>= return . not

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [Arg] -> IO String
pacmanOutput args = pacmanQuiet args >>= return . tripleSnd

syncDatabase :: Language -> IO ()
syncDatabase lang = do
  putStrLnA green $ syncDatabaseMsg1 lang
  hFlush stdout  -- This is experimental. Trying to fix a colour bug.
  pacman ["-Sy"]

packageCacheContents :: IO [String]
packageCacheContents = getDirectoryContents packageCache

getPacmanConf :: IO String
getPacmanConf = readFile pacmanConfFile

getIgnoredPkgs :: String -> [String]
getIgnoredPkgs confFile = words $ takeWhile (not . (==) '\n') field
    where (_,_,field) = confFile =~ pattern :: (String,String,String)
          pattern     = "^IgnorePkg[ ]+= "

getPacmanHelpMsg :: IO [String]
getPacmanHelpMsg = do
  helpMsg <- pacmanOutput ["-h"]
  return $ lines helpMsg

-- Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: IO [String]
getVersionInfo = do
  versionLines <- pacmanOutput ["-V"]
  return . map (drop lineHeaderLength) . lines $ versionLines

-- The amount of whitespace before text in the lines given by `pacman -V`
lineHeaderLength :: Int
lineHeaderLength = 23

