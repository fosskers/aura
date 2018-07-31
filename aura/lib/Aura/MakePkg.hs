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
import           BasePrelude
import           Control.Error.Util (note)
import qualified Data.List.NonEmpty as NEL
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           System.Path (Path, Absolute, fromAbsoluteFilePath, (</>), toFilePath)
import           System.Path.IO (getCurrentDirectory, getDirectoryContents)
import           System.Process.Typed

---

-- | The default location of the makepkg configuration: \/etc\/makepkg.conf
makepkgConfFile :: Path Absolute
makepkgConfFile = fromAbsoluteFilePath "/etc/makepkg.conf"

makepkgCmd :: Path Absolute
makepkgCmd = fromAbsoluteFilePath "/usr/bin/makepkg"

-- | Given the current user name, build the package of whatever
-- directory we're in.
makepkg :: Settings -> User -> IO (Either Failure (NonEmptySet (Path Absolute)))
makepkg ss usr = fmap g . f . make $ proc cmd (opts <> colour)
  where (cmd, opts) = runStyle usr . foldMap asFlag . makepkgFlagsOf $ buildConfigOf ss
        f | switch ss DontSuppressMakepkg = id
          -- TODO restore
          | otherwise = id -- print_stdout False . print_stderr False
        g (ExitSuccess, fs) = note (Failure buildFail_9) . fmap NES.fromNonEmpty $ NEL.nonEmpty fs
        g _ = Left $ Failure buildFail_8
        colour | shared ss (Colour Never)  = ["--nocolor"]
               | shared ss (Colour Always) = []
               | isTerminal ss = []
               | otherwise = ["--nocolor"] -- TODO is this right?

-- | Actually build the package, guarding on exceptions.
-- Yields the filepaths of the built package tarballs.
make :: ProcessConfig stdin stdout stderr -> IO (ExitCode, [Path Absolute])
make pc = do
  ec  <- runProcess pc
  pwd <- getCurrentDirectory
  fs  <- filter p . map (pwd </>) <$> getDirectoryContents pwd
  pure (ec, fs)
  where p (T.pack . toFilePath -> fp) = T.isSuffixOf "-x86_64.pkg.tar.xz" fp || T.isSuffixOf "-any.pkg.tar.xz" fp

-- | Make a source package. See `man makepkg` and grep for `--allsource`.
makepkgSource :: User -> IO [Path Absolute]
makepkgSource usr = do
  runProcess $ proc cmd opts
  pwd <- getCurrentDirectory
  filter (T.isSuffixOf ".src.tar.gz" . T.pack . toFilePath) . map (pwd </>) <$> getDirectoryContents pwd
    where (cmd, opts) = runStyle usr ["--allsource"]

-- | As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [String] -> (FilePath, [String])
runStyle (User usr) opts = ("sudo", ["-u", T.unpack usr, toFilePath makepkgCmd] <> opts)
