{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Socket
    ( subscribe
    , module Coinbase.Exchange.Types.Socket
    ) where

import           Data.Aeson
import           Network.Socket
import qualified Network.WebSockets             as WS
import           Wuss

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

subscribe :: ApiType -> ProductId -> WS.ClientApp a -> IO a
subscribe atype pid app = withSocketsDo $
        runSecureClient location 443 "/" $ \conn -> do
            WS.sendBinaryData conn $ encode (Subscribe pid)
            app conn
    where location = case atype of Sandbox -> sandboxSocket
                                   Live    -> liveSocket
