-- pacman interface.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Pacman where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

type Args = String

pacman :: [Args] -> IO (ExitCode,String,String)
pacman args = readProcessWithExitCode "pacman" args ""

-- Did a pacman process succeed?
pacmanSuccess :: [Args] -> IO Bool
pacmanSuccess args = do
  (exitStatus,_,_) <- pacman args
  case exitStatus of
    ExitSuccess -> return True
    _           -> return False

pacmanFailure :: [Args] -> IO Bool
pacmanFailure args = pacmanSuccess args >>= return . not
