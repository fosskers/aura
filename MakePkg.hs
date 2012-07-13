module MakePkg where

-- System Libraries
import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))
import System.Exit (ExitCode)

-- Custom Libraries
import Shell

makepkg :: String -> IO (ExitCode,FilePath,String)
makepkg = makepkgQuiet

-- This should to be used as non-root.
-- Building packages as root IS NOT safe!
makepkgGen :: (String -> [String] -> IO (ExitCode,String,String)) ->
              String -> IO (ExitCode,FilePath,String)
makepkgGen f user = do
  (exitStatus,out,err) <- f command opts
  contents <- getDirectoryContents "."  -- I don't like this relative path.
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar.xz")) contents
      pkgName  = if null pkgFiles then "" else head pkgFiles
  return $ (exitStatus,pkgName,out ++ "\n" ++ err)
      where (command,opts) = determineRunStyle user

determineRunStyle :: String -> (String,[String])
determineRunStyle "root" = ("makepkg",["--asroot"])
determineRunStyle user   = ("su",[user,"-c","makepkg"])

makepkgQuiet :: String -> IO (ExitCode,FilePath,String)
makepkgQuiet user = makepkgGen quietShellCmd' user

makepkgVerbose :: String -> IO (ExitCode,FilePath,String)
makepkgVerbose user = makepkgGen shellCmd' user
    where shellCmd' cmd opts = do
            exitStatus <- shellCmd cmd opts
            return (exitStatus,"","")
