module MakePkg where

import System.Exit (ExitCode)

makepkg :: FilePath -> IO (ExitCode,FilePath,String)
makepkg = undefined