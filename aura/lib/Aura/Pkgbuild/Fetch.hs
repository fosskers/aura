{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, DataKinds #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Query the AUR for a package's PKGBUILD.

module Aura.Pkgbuild.Fetch
  ( getPkgbuild
  , pkgbuildUrl
  ) where

import           Aura.Types (PkgName(..), Pkgbuild(..))
import           Aura.Utils (urlContents)
import           BasePrelude
import           Control.Exception (SomeException, catch)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Generics.Product (field)
import qualified Data.Text as T
import           Lens.Micro ((^.))
import           Network.HTTP.Client (Manager)
import           Network.URI (escapeURIString, isUnescapedInURIComponent)
import           System.FilePath ((</>))

---

type E = SomeException

baseUrl :: String
baseUrl = "https://aur.archlinux.org/"

-- | The location of a given package's PKGBUILD on the AUR servers.
pkgbuildUrl :: String -> String
pkgbuildUrl p = baseUrl </> "cgit/aur.git/plain/PKGBUILD?h="
  ++ escapeURIString isUnescapedInURIComponent p

-- | The PKGBUILD of a given package, retrieved from the AUR servers.
getPkgbuild :: MonadIO m => Manager -> PkgName -> m (Maybe Pkgbuild)
getPkgbuild m p = e $ do
  t <- urlContents m . pkgbuildUrl . T.unpack $ p ^. field @"name"
  pure $ fmap Pkgbuild t
  where e f = liftIO $ f `catch` (\(_ :: E) -> return Nothing)
