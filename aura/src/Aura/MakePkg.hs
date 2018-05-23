{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- Interface to `makepkg`.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

module Aura.MakePkg
    ( makepkg
    , makepkgSource
    , makepkgConfFile ) where

import Aura.Monad.Aura
import Aura.Settings.Base (suppressMakepkg, makepkgFlagsOf)
import Aura.Shell (shellCmd, quietShellCmd, quietShellCmd', checkExitCode')
import BasePrelude
import Shell (pwd, ls)
import Text.Regex.PCRE ((=~))

---

type User = String

makepkgConfFile :: FilePath
makepkgConfFile = "/etc/makepkg.conf"

makepkgCmd :: FilePath
makepkgCmd = "/usr/bin/makepkg"

makepkg :: Aura (String -> Aura [FilePath])
makepkg = (\quiet -> if quiet then makepkgQuiet else makepkgVerbose) <$>
              asks suppressMakepkg

-- TODO: Clean this up. Incompatible with `-x` and `--ignorearch`?
-- | Make a source package.
makepkgSource :: User -> Aura [FilePath]
makepkgSource user = do
  quietShellCmd cmd opts
  filter (=~ "[.]src[.]tar") <$> liftIO (pwd >>= ls)
    where (cmd, opts) = runStyle user ["--allsource"]

-- Builds a package with `makepkg`.
-- Some packages create multiple .pkg.tar files. These are all returned.
makepkgGen :: (String -> [String] -> Aura a) -> User -> Aura [FilePath]
makepkgGen make user = asks makepkgFlagsOf >>= \clfs -> do
    let (cmd, opts) = runStyle user clfs
    make cmd (opts <> clfs) *> (filter (=~ "[.]pkg[.]tar") <$> liftIO (pwd >>= ls))

-- As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [String] -> (String, [String])
runStyle user opts = ("sudo", ["-u", user, makepkgCmd] <> opts)

makepkgQuiet :: User -> Aura [FilePath]
makepkgQuiet = makepkgGen quiet
    where quiet cmd opts = do
            (status, out, err) <- quietShellCmd' cmd opts
            let output = err <> "\n" <> out
            checkExitCode' output status

makepkgVerbose :: User -> Aura [FilePath]
makepkgVerbose = makepkgGen shellCmd
