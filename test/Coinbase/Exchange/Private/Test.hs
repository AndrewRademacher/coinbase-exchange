{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Private.Test
    ( tests
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import           Data.Time
import           Data.UUID.V4
import           System.IO.Unsafe
import           System.Locale
import           Test.Tasty
import           Test.Tasty.HUnit

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

accountId :: IORef (Maybe AccountId)
accountId = unsafePerformIO $ newIORef Nothing
{-# NOINLINE accountId #-}

cancelOrderId :: IORef (Maybe OrderId)
cancelOrderId = unsafePerformIO $ newIORef Nothing
{-# NOINLINE cancelOrderId #-}

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Private"
        [ case_parse conf "getAccountList" $ getAccountList >>= liftIO . writeIORef accountId . Just . accId . head
        , case_parse conf "getAccount" $ liftIO (readIORef accountId) >>= getAccount . fromJust
        , case_parse conf "getAccountLedger" $ liftIO (readIORef accountId) >>= getAccountLedger . fromJust
        , case_placeOrder conf
        , case_cancelOrder conf
        ]

case_parse :: ExchangeConf -> String -> Exchange a -> TestTree
case_parse conf l fn = testCase l $ do
        v <- liftIO $ runExchange conf fn
        case v of Left  e -> print e
                  Right _ -> return ()
        assertBool "Failed to parse."
            (case v of
                Left  _ -> False
                Right _ -> True)

case_placeOrder :: ExchangeConf -> TestTree
case_placeOrder conf = testCase "place an order" $ do
        res <- liftIO $ runExchange conf $
            createOrder NewOrder
                { noSize      = 1
                , noPrice     = 1
                , noSide      = Buy
                , noProductId = "BTC-USD"
                , noClientOid = Nothing
                , noSelfTrade = Nothing
                }
        case res of
            Left  e -> print e >> assertFailure "Unable to place order."
            Right v -> writeIORef cancelOrderId (Just v)

case_cancelOrder :: ExchangeConf -> TestTree
case_cancelOrder conf = testCase "cancel an order" $ do
        oid <- liftM fromJust $ readIORef cancelOrderId
        res <- liftIO $ runExchange conf $ cancelOrder oid
        case res of
            Left  e -> print e >> assertFailure "Unable to cancel order."
            Right _ -> return ()
