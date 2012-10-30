{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.MakePkg where

-- System Libraries
import Text.Regex.PCRE ((=~))
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
  contents <- pwd >>= ls
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar.xz")) contents
      pkgName  = if null pkgFiles then "" else head pkgFiles
  return $ (exitStatus,pkgName,err ++ "\n" ++ out)
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
