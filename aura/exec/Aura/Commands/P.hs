{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- |
-- Module    : Aura.Commands.P
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
--
-- Handles @-P@ flags for analysing PKGBUILDs.

module Aura.Commands.P
  ( exploitsFromFile
  , exploitsFromStdin
  , audit
  ) where

import           Aura.Core
import           Aura.Languages
import           Aura.Pkgbuild.Fetch
import           Aura.Pkgbuild.Security
import           Aura.Security
import           Aura.Settings
import           Aura.Types
import           Control.Scheduler (Comp(..), traverseConcurrently)
import           RIO
import           RIO.ByteString (getContents)
import qualified RIO.Set as S

---

-- | Given a specific file path, print out any potential PKGBUILD exploits and
-- exit the program aggressively to force a naughty exit code.
exploitsFromFile :: FilePath -> RIO Env ()
exploitsFromFile = readFileBinary >=> findExploits . Pkgbuild

exploitsFromStdin :: RIO Env ()
exploitsFromStdin = getContents >>= findExploits . Pkgbuild

-- | Analyse all locally installed AUR packages.
audit :: RIO Env ()
audit = do
  ss <- asks settings
  let !m = managerOf ss
  ps <- liftIO foreignPackages
  warn ss . security_13 . fromIntegral $ S.size ps
  pbs <- catMaybes <$> liftIO (traverseConcurrently Par' (getPkgbuild m . spName) $ S.toList ps)
  mapMaybeA f pbs >>= \case
    [] -> notify ss security_14
    _  -> throwM $ Failure security_12
  where
    f :: Pkgbuild -> RIO Env (Maybe Pkgbuild)
    f pb = case parsedPB pb of
      Nothing -> pure Nothing
      Just l -> case bannedTerms l of
        [] -> pure Nothing
        bts -> do
          ss <- asks settings
          liftIO $ traverse_ (displayBannedTerms ss) bts
          pure $ Just pb

findExploits :: Pkgbuild -> RIO Env ()
findExploits pb = case parsedPB pb of
  Nothing -> throwM $ Failure security_11
  Just l  -> case bannedTerms l of
    []  -> pure ()
    bts -> do
      ss <- asks settings
      liftIO $ traverse_ (displayBannedTerms ss) bts
      throwM $ Failure security_12
