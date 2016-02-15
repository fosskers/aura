{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

-- Interface to `makepkg`.

{-

Copyright 2012, 2013, 2014, 2015 Colin Woodbury <colingw@gmail.com>

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

import           BasicPrelude hiding (FilePath)

import           Shelly
import           Filesystem.Path.CurrentOS

import           Utilities
import           Aura.Shell

---

type User = Text

findPkgFile :: FilePath -> Sh [FilePath]
findPkgFile = findDirFilterWhen (pure . const False) (pure . matches)
  where matches fp = ["xz", "tar", "pkg"] `isPrefixOf` reverse (extensions fp)

makepkgConfFile :: Text
makepkgConfFile = "/etc/makepkg.conf"

makepkgCmd :: Text
makepkgCmd = "/usr/bin/makepkg"

makepkg :: Bool -> User -> [Text] -> Sh (Either Text [FilePath])
makepkg True  = makepkgQuiet
makepkg False = makepkgVerbose

-- TODO: Clean this up. Incompatible with `-x` and `--ignorearch`?
-- `run_` failing here isn't caught yet. Do properly with EEs.
-- | Make a source package.
makepkgSource :: User -> Sh [FilePath]
makepkgSource user = do
  run_ com opts
  (pwd >>= findPkgFile)
    where (com, opts) = runStyle user ["--allsource"]

-- | Builds a package with `makepkg`.
-- Some packages create multiple .pkg.tar files. These are all returned.
makepkgGen :: (FilePath -> [Text] -> Sh (Either Text ())) -> User -> [Text] -> Sh (Either Text [FilePath])
makepkgGen make user clfs = do
    let (com, opts) = runStyle user clfs
    r <- make com (opts <> clfs)
    case r of
      Left err -> pure $ Left err
      Right _  -> Right <$> (pwd >>= findPkgFile)

-- As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [Text] -> (FilePath, [Text])
runStyle user opts = ("sudo", ["-u", user, makepkgCmd] <> opts)

makepkgQuiet :: User -> [Text] -> Sh (Either Text [FilePath])
makepkgQuiet = makepkgGen quiet
  where quiet com opts = do
          output <- run com opts
          stderr <- lastStderr
          ifte_ (Right ()) (Left $ stderr <> "\n" <> output) <$> wasSuccessful

makepkgVerbose :: User -> [Text] -> Sh (Either Text [FilePath])
makepkgVerbose = makepkgGen verbose
  where verbose com opts = do
          run_ com opts
          ifte_ (Right ()) (Left "") <$> wasSuccessful -- TODO: stderr?
