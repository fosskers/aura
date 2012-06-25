module MakePkg where

import System.Process (readProcessWithExitCode, rawSystem)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)

makepkg = makepkgQuiet

-- This needs to be run as non-root!
-- Building packages as root IS NOT safe!
makepkgGen :: (String -> [String] -> String -> IO (ExitCode,String,String)) ->
           String -> [String] -> IO (ExitCode,FilePath,String)
makepkgGen f user opts = do
  let command = "su"
      child   = unwords $ "makepkg" : opts 
      suOpts  = [user,"-c",child]
  (exitStatus,out,err) <- f command suOpts ""
  contents <- getDirectoryContents "."  -- I don't like this relative path.
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar.xz")) contents
      pkgName  = if null pkgFiles then "" else head pkgFiles
  return $ (exitStatus,pkgName,out ++ "\n" ++ err)

makepkgQuiet :: String -> [String] -> IO (ExitCode,FilePath,String)
makepkgQuiet user opts = makepkgGen readProcessWithExitCode user opts

makepkgVerbose :: String -> [String] -> IO (ExitCode,FilePath,String)
makepkgVerbose user opts = makepkgGen rawSystem' user opts
    where rawSystem' c os _ = do
            exitStatus <- rawSystem c os
            return (exitStatus,"","")
