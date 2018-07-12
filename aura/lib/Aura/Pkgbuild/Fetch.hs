{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Query the AUR for a package's PKGBUILD.

module Aura.Pkgbuild.Fetch
  ( pkgbuild
  , pkgbuildUrl
  ) where

import           BasePrelude
import           Control.Exception (SomeException, catch)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Text
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding
import           Network.HTTP.Client (Manager)
import           Network.URI (escapeURIString, isUnescapedInURIComponent)
import           System.FilePath ((</>))
import           Utilities (urlContents)

---

type E = SomeException

baseUrl :: String
baseUrl = "https://aur.archlinux.org/"

-- | The location of a given package's PKGBUILD on the AUR servers.
pkgbuildUrl :: String -> String
pkgbuildUrl p = baseUrl </> "cgit/aur.git/plain/PKGBUILD?h="
  ++ escapeURIString isUnescapedInURIComponent p

-- | The PKGBUILD of a given package, retrieved from the AUR servers.
pkgbuild :: MonadIO m => Manager -> Text -> m (Maybe Text)
pkgbuild m (unpack -> p) = e $ do
  t <- urlContents m $ pkgbuildUrl p
  pure $ fmap (TL.toStrict . decodeUtf8With lenientDecode) t
  where e f = liftIO $ f `catch` (\(_ :: E) -> return Nothing)
