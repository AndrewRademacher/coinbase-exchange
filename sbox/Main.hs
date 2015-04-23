{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Types

main :: IO ()
main = putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr <- newManager tlsManagerSettings
        res <- runExchange (ExchangeConf mgr) act
        case res of
            Right s -> return s
            Left  f -> error $ show f
