{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.MarketData.Test
    ( tests
    ) where

import           Control.Monad.IO.Class
import           Data.Time
import           Test.Tasty
import           Test.Tasty.HUnit

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

tests :: ExchangeConf -> TestTree
tests conf = testGroup "MarketData Parse"
        [ case_parse conf "getProducts" getProducts

        , case_parse conf "getTopOfBook" $ getTopOfBook defProduct
        , case_parse conf "getTop50OfBook" $ getTop50OfBook defProduct
        , case_parse conf "getOrderBook" $ getOrderBook defProduct

        , case_parse conf "getProductTicker" $ getProductTicker defProduct
        , case_parse conf "getTrades" $ getTrades defProduct

        , case_parse conf "getHistory" $ getHistory defProduct defStart defEnd (Just 10000)

        , case_parse conf "getCurrencies" getCurrencies
        , case_parse conf "getExchangeTime" getExchangeTime
        ]

defProduct :: ProductId
defProduct = ProductId "BTC-USD"

defStart :: Maybe UTCTime
defStart = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

defEnd :: Maybe UTCTime
defEnd = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

case_parse :: ExchangeConf -> String -> Exchange a -> TestTree
case_parse conf l fn = testCase l $ do
        v <- liftIO $ runExchange conf fn
        assertBool "Failed to parse."
            (case v of
                Left  _ -> False
                Right _ -> True)
