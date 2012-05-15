-- pacman interface.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

import System.Process (readProcessWithExitCode, rawSystem)
import System.Exit (ExitCode(..))

type Args = String

pacman :: [Args] -> IO ExitCode
pacman args = rawSystem "pacman" args

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

getPacmanHelpMsg :: IO [String]
getPacmanHelpMsg = do
  (_,helpMsg,_) <- pacmanQuiet ["-h"]
  return $ lines helpMsg
