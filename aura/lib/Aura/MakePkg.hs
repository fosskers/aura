{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Interface to `makepkg`.

module Aura.MakePkg
  ( makepkg
  , makepkgSource
  , makepkgConfFile
  ) where

import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (exitCode)
import           BasePrelude hiding (FilePath)
import           Control.Error.Util (note)
import qualified Data.List.NonEmpty as NEL
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Shelly hiding (cmd)

---

-- | The default location of the makepkg configuration: \/etc\/makepkg.conf
makepkgConfFile :: FilePath
makepkgConfFile = "/etc/makepkg.conf"

makepkgCmd :: T.Text
makepkgCmd = "/usr/bin/makepkg"

-- | Given the current user name, build the package of whatever
-- directory we're in.
makepkg :: Settings -> User -> Sh (Either Failure (NonEmptySet FilePath))
makepkg ss user = fmap g . f $ make cmd (opts <> colour)
  where (cmd, opts) = runStyle user . foldMap asFlag . makepkgFlagsOf $ buildConfigOf ss
        f | switch ss DontSuppressMakepkg = id
          | otherwise = print_stdout False . print_stderr False
        g (ExitSuccess, fs) = note (Failure buildFail_9) . fmap NES.fromNonEmpty $ NEL.nonEmpty fs
        g _ = Left $ Failure buildFail_8
        colour | shared ss (Colour Never)  = ["--nocolor"]
               | shared ss (Colour Always) = []
               | isTerminal ss = []
               | otherwise = ["--nocolor"]

-- | Actually build the package, guarding on exceptions.
-- Yields the filepaths of the built package tarballs.
make :: FilePath -> [T.Text] -> Sh (ExitCode, [FilePath])
make cmd opts = errExit False $ do
  run_ cmd opts
  fs <- filter p <$> (pwd >>= ls)
  ec <- exitCode <$> lastExitCode
  pure (ec, fs)
  where p (toTextIgnore -> fp) = T.isSuffixOf "-x86_64.pkg.tar.xz" fp || T.isSuffixOf "-any.pkg.tar.xz" fp

-- | Make a source package. See `man makepkg` and grep for `--allsource`.
makepkgSource :: User -> Sh [FilePath]
makepkgSource user = do
  run_ cmd opts
  filter (T.isSuffixOf ".src.tar.gz" . toTextIgnore) <$> (pwd >>= ls)
    where (cmd, opts) = runStyle user ["--allsource"]

-- | As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [T.Text] -> (FilePath, [T.Text])
runStyle (User user) opts = ("sudo", ["-u", user, makepkgCmd] <> opts)
