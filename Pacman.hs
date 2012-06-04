-- pacman interface.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

import System.Process (readProcessWithExitCode, rawSystem)
import System.Exit (ExitCode(..))
import Text.Regex.Posix ((=~))
import Utilities (tripleSnd)

type Args = String

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

pacman :: [Args] -> IO ()
pacman args = rawSystem "pacman" args >> return ()

-- Runs pacman without producing any output.
pacmanQuiet :: [Args] -> IO (ExitCode,String,String)
pacmanQuiet args = readProcessWithExitCode "pacman" args ""

-- Did a pacman process succeed?
pacmanSuccess :: [Args] -> IO Bool
pacmanSuccess args = do
  (exitStatus,_,_) <- pacmanQuiet args
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False

pacmanFailure :: [Args] -> IO Bool
pacmanFailure args = pacmanSuccess args >>= return . not

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [Args] -> IO String
pacmanOutput args = pacmanQuiet args >>= return . tripleSnd

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

