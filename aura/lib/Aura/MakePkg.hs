{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import           Aura.Utils                 (optionalPrompt, strictText)
import           BasePrelude
import           Control.Error.Util         (note)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.NonEmpty         as NEL
import           Data.Set.NonEmpty          (NonEmptySet)
import qualified Data.Set.NonEmpty          as NES
import qualified Data.Text                  as T
import           Lens.Micro                 ((^.), _2)
import           System.Path                (Absolute, Path,
                                             fromAbsoluteFilePath, toFilePath,
                                             (</>))
import           System.Path.IO             (getCurrentDirectory,
                                             getDirectoryContents)
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
makepkg ss usr = make ss usr (proc cmd $ opts <> colour) >>= g
  where (cmd, opts) = runStyle usr . foldMap asFlag . makepkgFlagsOf $ buildConfigOf ss
        g (ExitSuccess, _, fs)   = pure . note (Failure buildFail_9) . fmap NES.fromNonEmpty $ NEL.nonEmpty fs
        g (ExitFailure _, se, _) = do
          unless (switch ss DontSuppressMakepkg) $ do
            showError <- optionalPrompt ss buildFail_11
            when showError $ BL.putStrLn se
          pure . Left $ Failure buildFail_8
        colour | shared ss (Colour Never)  = ["--nocolor"]
               | shared ss (Colour Always) = []
               | isTerminal ss = []
               | otherwise = ["--nocolor"]

-- | Actually build the package, guarding on exceptions.
-- Yields the filepaths of the built package tarballs.
make :: MonadIO m => Settings -> User -> ProcessConfig stdin stdout stderr -> m (ExitCode, BL.ByteString, [Path Absolute])
make ss (User usr) pc = do
  (ec, se) <- runIt ss pc
  res <- readProcess $ proc "sudo" ["-u", T.unpack usr, toFilePath makepkgCmd, "--packagelist"]
  let fs = map (fromAbsoluteFilePath . T.unpack . strictText) . BL.lines $ res ^. _2
  pure (ec, se, fs)

runIt :: MonadIO m => Settings -> ProcessConfig stdin stdout stderr -> m (ExitCode, BL.ByteString)
runIt ss pc | switch ss DontSuppressMakepkg = (,mempty) <$> runProcess pc
            | otherwise = (\(ec, _, se) -> (ec, se)) <$> readProcess pc

-- | Make a source package. See `man makepkg` and grep for `--allsource`.
makepkgSource :: User -> IO [Path Absolute]
makepkgSource usr = do
  void . runProcess $ proc cmd opts
  pwd <- getCurrentDirectory
  filter (T.isSuffixOf ".src.tar.gz" . T.pack . toFilePath) . map (pwd </>) <$> getDirectoryContents pwd
    where (cmd, opts) = runStyle usr ["--allsource"]

-- | As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [String] -> (FilePath, [String])
runStyle (User usr) opts = ("sudo", ["-u", T.unpack usr, toFilePath makepkgCmd] <> opts)
