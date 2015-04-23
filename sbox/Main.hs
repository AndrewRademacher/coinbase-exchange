{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Locale

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Types

main :: IO ()
main = putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ readTime defaultTimeLocale "%FT%X%z" "2014-04-23T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr <- newManager tlsManagerSettings
        res <- runExchange (ExchangeConf mgr) act
        case res of
            Right s -> return s
            Left  f -> error $ show f
