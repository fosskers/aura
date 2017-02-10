{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module    : Linux.Arch.Aur.Rpc
-- Copyright : (c) Colin Woodbury, 2014, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- See https://aur.archlinux.org/rpc for details.

module Linux.Arch.Aur.Rpc
       ( info, search ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy
import Data.Text (Text)
import Linux.Arch.Aur.Types
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

---

type Info = "rpc" :> QueryParam "v" Text
           :> QueryParam "type" Text
           :> QueryParams "arg[]" Text
           :> Get '[JSON] RPCResp

type Search = "rpc" :> QueryParam "v" Text
           :> QueryParam "type" Text
           :> QueryParam "arg" Text
           :> Get '[JSON] RPCResp

type API = Info :<|> Search

api :: Proxy API
api = Proxy

url :: BaseUrl
url = BaseUrl Https "aur.archlinux.org" 443 ""

-- | Make a call to the AUR RPC. Assumes version 5 of the API.
rpcI :<|> rpcS = client api

-- | Perform an @info@ call on one or more package names.
info :: MonadIO m => Manager -> [Text] -> m [AurInfo]
info m ps = unwrap m $ rpcI (Just "5") (Just "info") ps

-- | Perform a @search@ call on a package name or description text.
search :: MonadIO m => Manager -> Text -> m [AurInfo]
search m p = unwrap m $ rpcS (Just "5") (Just "search") (Just p)

unwrap :: MonadIO m => Manager -> ClientM RPCResp -> m [AurInfo]
unwrap m r = liftIO . fmap (either (const []) _results) $ runClientM r (ClientEnv m url)
