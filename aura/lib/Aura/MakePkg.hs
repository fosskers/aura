{-# LANGUAGE TupleSections #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Interface to `makepkg`.

module Aura.MakePkg
  ( makepkg
  , makepkgSource
  ) where

import           Aura.IO (optionalPrompt)
import           Aura.Languages
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (note)
import           Lens.Micro (_2)
import           RIO
import qualified RIO.ByteString.Lazy as BL
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import           System.Process.Typed

---

makepkgCmd :: FilePath
makepkgCmd = "/usr/bin/makepkg"

-- | Given the current user name, build the package of whatever
-- directory we're in.
makepkg :: Settings -> User -> IO (Either Failure (NonEmpty FilePath))
makepkg ss usr = make ss usr (proc cmd $ opts <> colour) >>= g
  where
    (cmd, opts) = runStyle usr . map T.unpack . foldMap asFlag . makepkgFlagsOf $ buildConfigOf ss
    g (ExitSuccess, _, fs)   = pure . note (Failure buildFail_9) $ NEL.nonEmpty fs
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
make :: MonadIO m
  => Settings
  -> User
  -> ProcessConfig stdin stdout stderr
  -> m (ExitCode, BL.ByteString, [FilePath])
make ss (User usr) pc = do
  (ec, se) <- runIt ss pc
  res <- readProcess $ proc "sudo" ["-u", T.unpack usr, makepkgCmd, "--packagelist"]
  let fs = map T.unpack . T.lines . decodeUtf8Lenient . BL.toStrict $ res ^. _2
  pure (ec, se, fs)

runIt :: MonadIO m
  => Settings
  -> ProcessConfig stdin stdout stderr
  -> m (ExitCode, BL.ByteString)
runIt ss pc | switch ss DontSuppressMakepkg = (,mempty) <$> runProcess pc
            | otherwise = (\(ec, _, se) -> (ec, se)) <$> readProcess pc

-- | Make a source package. See `man makepkg` and grep for `--allsource`.
makepkgSource :: User -> IO [FilePath]
makepkgSource usr = do
  void . runProcess $ proc cmd opts
  pwd <- getCurrentDirectory
  filter (T.isSuffixOf ".src.tar.gz" . T.pack) . map (pwd </>) <$> listDirectory pwd
    where (cmd, opts) = runStyle usr ["--allsource"]

-- | As of makepkg v4.2, building with `--asroot` is no longer allowed.
runStyle :: User -> [String] -> (FilePath, [String])
runStyle (User usr) opts = ("sudo", ["-u", T.unpack usr, makepkgCmd] <> opts)
