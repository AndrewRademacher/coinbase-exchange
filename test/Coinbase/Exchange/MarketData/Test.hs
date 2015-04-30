module Coinbase.Exchange.MarketData.Test
    ( tests
    ) where

import Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.MarketData

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Market Data"
        [ case_parseGetCurrencies conf
        , case_parseGetExchangeTime conf
        ]

case_parseGetCurrencies :: ExchangeConf -> TestTree
case_parseGetCurrencies conf = testCase "Parse get currencies." $ do
        v <- liftIO $ runExchange conf getCurrencies
        assertBool "Failed to parse exchange time."
            (case v of
                Left  _ -> False
                Right _ -> True)

case_parseGetExchangeTime :: ExchangeConf -> TestTree
case_parseGetExchangeTime conf = testCase "Parse exchange time." $ do
        v <- liftIO $ runExchange conf getExchangeTime
        assertBool "Failed to parse exchange time."
            (case v of
                Left  _ -> False
                Right _ -> True)
