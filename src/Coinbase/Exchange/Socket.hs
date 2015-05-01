{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Socket where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Network.Socket
import qualified Network.WebSockets             as WS
import           Wuss

import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

btc :: ProductId
btc = "BTC-USD"

app :: WS.ClientApp ()
app conn = do
        putStrLn "Connected"
        _ <- forkIO $ forever $ do
            ds <- WS.receiveData conn
            let res = eitherDecode ds
            print (res :: Either String ExchangeMessage)
        WS.sendBinaryData conn $ encode (Subscribe btc)
        _ <- forever $ threadDelay (1000000 * 60)
        return ()

defaultMain :: IO ()
defaultMain = withSocketsDo $
    runSecureClient "ws-feed.exchange.coinbase.com" 443 "/" app
