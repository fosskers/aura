module MakePkg where

import System.Process (readProcessWithExitCode)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)

-- This needs to be run as non-root!
-- Building packages as non-root IS NOT safe!
makepkg :: String -> [String] -> IO (ExitCode,FilePath,String)
makepkg user opts = do
  let command = "su"
      child   = unwords $ "makepkg" : opts 
      suOpts    = [user,"-c",child]
  (exitStatus,out,err) <- readProcessWithExitCode command suOpts ""
  contents <- getDirectoryContents "."
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar.xz" :: Bool)) contents
      pkgName  = if null pkgFiles then "" else head pkgFiles
  return $ (exitStatus,pkgName,out ++ "\n" ++ err)
