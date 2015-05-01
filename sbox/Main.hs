{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.Socket                 (withSocketsDo)
import qualified Network.WebSockets             as WS
import           System.Locale
import           Wuss

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr <- newManager tlsManagerSettings
        res <- runExchange (ExchangeConf mgr) act
        case res of
            Right s -> return s
            Left  f -> error $ show f
