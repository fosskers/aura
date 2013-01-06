-- Interface to `makepkg`.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

import Text.Regex.PCRE ((=~))

import Aura.Monad.Aura
import Aura.Shell (shellCmd, quietShellCmd', checkExitCode')

import Shell (pwd, ls)

---

-- This should to be used as non-root.
-- Building packages as root IS NOT safe!
makepkgGen :: (String -> [String] -> Aura a) -> String -> Aura FilePath
makepkgGen f user = do
  _ <- f command opts
  files  <- liftIO (pwd >>= ls)
  let pkgFiles = filter (\file -> (file =~ ".pkg.tar")) files
  return $ if null pkgFiles then "" else head pkgFiles
      where (command,opts) = determineRunStyle user

determineRunStyle :: String -> (String,[String])
determineRunStyle "root" = ("makepkg",["--asroot"])
determineRunStyle user   = ("su",[user,"-c","makepkg"])

makepkgQuiet :: String -> Aura FilePath
makepkgQuiet user = makepkgGen quiet user
    where quiet cmd opts = do
            (status,out,err) <- quietShellCmd' cmd opts
            let output = err ++ "\n" ++ out
            checkExitCode' output status

makepkgVerbose :: String -> Aura FilePath
makepkgVerbose user = makepkgGen verbose user
    where verbose cmd opts = shellCmd cmd opts
