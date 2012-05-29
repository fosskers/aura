module MakePkg where

import System.Process (readProcessWithExitCode)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)

makepkg :: [String] -> IO (ExitCode,FilePath,String)
makepkg opts = do
  (exitStatus,out,err) <- readProcessWithExitCode "makepkg" opts ""
  contents <- getDirectoryContents "."
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar.xz" :: Bool)) contents
      pkgName  = if null pkgFiles then "" else head pkgFiles
  return $ (exitStatus,pkgName,out ++ "\n" ++ err)