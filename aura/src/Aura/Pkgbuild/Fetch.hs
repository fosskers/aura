{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-

Copyright 2012, 2013, 2014, 2016 Colin Woodbury <colingw@gmail.com>

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

module Aura.Pkgbuild.Fetch
       ( pkgbuild
       , pkgbuild'
       , pkgbuildUrl ) where

import           BasePrelude
import           Control.Exception (SomeException, catch)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Text hiding (take)
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding
import           Internet
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
pkgbuild :: MonadIO m => Manager -> String -> m (Maybe Text)
pkgbuild m p = e $ do
  t <- urlContents m $ pkgbuildUrl p
  pure $ fmap (TL.toStrict . decodeUtf8With lenientDecode) t
  where e f = liftIO $ f `catch` (\(_ :: E) -> return Nothing)

-- | Callable with Text as well, if that's easier.
pkgbuild' :: MonadIO m => Manager -> Text -> m (Maybe Text)
pkgbuild' m (unpack -> p) = pkgbuild m p
