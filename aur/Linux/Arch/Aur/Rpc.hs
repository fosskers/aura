{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module    : Linux.Arch.Aur.Rpc
-- Copyright : (c) Colin Woodbury, 2014 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- See https://aur.archlinux.org/rpc for details.

module Linux.Arch.Aur.Rpc ( info, search ) where

import Control.Error.Util (hush)
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
-- Will fail with a `Nothing` if there was a connection/decoding error.
info :: Manager -> [Text] -> IO (Maybe [AurInfo])
info m ps = unwrap m $ rpcI (Just "5") (Just "info") ps

-- | Perform a @search@ call on a package name or description text.
-- Will fail with a `Nothing` if there was a connection/decoding error.
search :: Manager -> Text -> IO (Maybe [AurInfo])
search m p = unwrap m $ rpcS (Just "5") (Just "search") (Just p)

unwrap :: Manager -> ClientM RPCResp -> IO (Maybe [AurInfo])
unwrap m r = fmap _results . hush <$> runClientM r (ClientEnv m url Nothing)
