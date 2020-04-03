{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2020
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
import           Network.HTTP.Client (Manager)
import           Network.URI (escapeURIString, isUnescapedInURIComponent)
import           RIO
import           RIO.FilePath ((</>))
import qualified RIO.Text as T

---

baseUrl :: String
baseUrl = "https://aur.archlinux.org/"

-- | The location of a given package's PKGBUILD on the AUR servers.
pkgbuildUrl :: String -> String
pkgbuildUrl p = baseUrl </> "cgit/aur.git/plain/PKGBUILD?h="
  ++ escapeURIString isUnescapedInURIComponent p

-- | The PKGBUILD of a given package, retrieved from the AUR servers.
getPkgbuild :: Manager -> PkgName -> IO (Maybe Pkgbuild)
getPkgbuild m p = e $ do
  t <- urlContents m . pkgbuildUrl . T.unpack $ pnName p
  pure $ fmap Pkgbuild t
  where
    -- TODO Make this less "baby's first Haskell".
    e :: IO (Maybe a) -> IO (Maybe a)
    e f = f `catch` (\(_ :: SomeException) -> return Nothing)
