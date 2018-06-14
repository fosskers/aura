{-# LANGUAGE OverloadedStrings #-}

-- Interface to `makepkg`.

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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
  , makepkgConfFile
  ) where

import           Aura.Languages
import           Aura.Settings.Base
import           Aura.Types
import           BasePrelude hiding (FilePath)
import qualified Data.Text as T
import           Shelly hiding (cmd)
import           Utilities (User(..), exitCode)

---

makepkgConfFile :: FilePath
makepkgConfFile = "/etc/makepkg.conf"

makepkgCmd :: T.Text
makepkgCmd = "/usr/bin/makepkg"

-- | Given the current user name, build the package of whatever
-- directory we're in.
makepkg :: Settings -> User -> Sh (Either Failure [FilePath])
makepkg ss user = fmap g . f $ make cmd opts
  where (cmd, opts) = runStyle user . map asFlag . toList . makepkgFlagsOf $ buildConfigOf ss
        f | switch ss DontSuppressMakepkg = id
          | otherwise = print_stdout False . print_stderr False
        g (ExitSuccess, fs) = Right fs
        g _ = failure buildFail_8

-- | Actually build the package, guarding on exceptions.
-- Yields the filepaths of the built package tarballs.
make :: FilePath -> [T.Text] -> Sh (ExitCode, [FilePath])
make cmd opts = errExit False $ do
  run_ cmd opts
  fs <- filter (T.isSuffixOf ".pkg.tar.xz" . toTextIgnore) <$> (pwd >>= ls)
  ec <- exitCode <$> lastExitCode
  pure (ec, fs)

-- TODO: Clean this up. Incompatible with `-x` and `--ignorearch`?
-- | Make a source package. See `man makepkg` and grep for
-- `--allsource`.
makepkgSource :: User -> Sh [FilePath]
makepkgSource user = do
  run_ cmd opts
  filter (T.isSuffixOf ".src.tar.gz" . toTextIgnore) <$> (pwd >>= ls)
    where (cmd, opts) = runStyle user ["--allsource"]

-- As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [T.Text] -> (FilePath, [T.Text])
runStyle (User user) opts = ("sudo", ["-u", user, makepkgCmd] <> opts)
