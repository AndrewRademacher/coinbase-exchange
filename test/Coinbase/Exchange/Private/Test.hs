{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Coinbase.Exchange.Private.Test
    ( tests
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import           Data.List
import           Data.Time
import           Data.Scientific
import           Data.UUID.V4

import           System.Random
import           Control.Concurrent

import           Test.Tasty
import           Test.Tasty.HUnit

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

deriving instance Eq Order

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Private"
        [ testCase "getAccountList"     (do as <- case_getAccountList conf
                                            case as of
                                                [] -> error "Received empty list of accounts"
                                                _  -> return ()
                                        )

        , testCase "getUSDAccount"      (do as <- case_getAccountList conf
                                            let usdAccount = findUSDAccount as
                                            ac <- case_getAccount conf (accId usdAccount)
                                            assertEqual "accounts match" usdAccount ac
                                        )

        ,testCase "getUSDAccountLedger" (do as <- case_getAccountList conf
                                            let usdAccount = findUSDAccount as
                                            es <- case_getAccountLedger conf (accId usdAccount)
                                            case es of
                                                [] -> assertFailure "Received empty list of ledger entries" -- must not be empty to test parser
                                                _  -> return ()
                                        )

        , testCase "placeOrder"         (do o   <- creatNewOrder
                                            oid <- case_placeOrder conf o
                                            return ()
                                        )

        , testCase "getOrderList"       (do case_getOrderList conf [Open, Pending] -- making sure this request is well formed
                                            os <- case_getOrderList conf []
                                            case os of
                                                [] -> assertFailure "Received empty order list"
                                                _  -> return ()
                                        )

        , testCase "getOrder"           (do no  <- creatNewOrder
                                            oid <- case_placeOrder conf no
                                            o   <- case_getOrder conf oid
                                            assertEqual "order price"    (noPrice no) (orderPrice o)
                                            assertEqual "order size"     (noSize  no) (orderSize  o)
                                            assertEqual "order side"     (noSide  no) (orderSide  o)
                                            assertEqual "order product"  (noProductId no) (orderProductId o)
                                        )
        , testCase "cancelOrder"        (do no  <- creatNewOrder
                                            oid <- case_placeOrder  conf no
                                            os  <- case_getOrderList conf [Open, Pending]
                                            case_cancelOrder conf oid
                                            os' <- case_getOrderList conf [Open, Pending]
                                            case os \\ os' of
                                                            [o] -> do
                                                                    assertEqual "order price"    (noPrice no) (orderPrice o)
                                                                    assertEqual "order size"     (noSize  no) (orderSize  o)
                                                                    assertEqual "order side"     (noSide  no) (orderSide  o)
                                                                    assertEqual "order product"  (noProductId no) (orderProductId o)

                                                            [] -> assertFailure "order not canceled"
                                                            _  -> assertFailure "more than one order canceled"
                                        )
        , testCase "getFills"           (do oid <- case_placeOrder conf giveAwayOrder
                                            putStrLn $ "OID: " ++ show oid
                                            os <- case_getOrderList conf []
                                            threadDelay $ 1000 * 1000 * 3
                                            fs <- case_getFills conf Nothing Nothing  -- FIX ME!!! (Just oid)
                                            putStrLn $ "Fills: " ++ show fs
                                            print os

                                            case fs of
                                                [] -> assertFailure "No fills found for an order that should execute immediately"
                                                _  -> return ()
                                        )
        ]

-----------------------------------------------
giveAwayOrder :: NewOrder
giveAwayOrder = NewOrder
    { noSize      = 0.1
    , noPrice     = 10 -- super cheap! This *will* execute
    , noSide      = Sell
    , noProductId = "BTC-USD"
    , noClientOid = Nothing
    , noSelfTrade = Nothing
    }

creatNewOrder :: IO NewOrder
creatNewOrder = do
    -- can't be deterministic because exchange is stateful
    -- running a test twice with same random input may produce different results
    sz <- randomRIO (0,9999)
    return NewOrder
        { noSize      = 1 + Size (CoinScientific $ fromInteger sz / 10000 )
        , noPrice     = 10
        , noSide      = Buy
        , noProductId = "BTC-USD"
        , noClientOid = Nothing
        , noSelfTrade = Nothing
        }

findUSDAccount :: [Account] -> Account
findUSDAccount as = case filter (\a -> accCurrency a == CurrencyId "USD") as of
                        [a] -> a
                        []  -> error "no USD denominated account found"
                        _   -> error "more than one account denominated in USD found"

-----------------------------------------------
onSuccess :: ExchangeConf -> Exchange a -> String -> IO a
onSuccess conf apicall errorstring = do
        r <- liftIO $ runExchange conf apicall
        case r of
            Left  e -> print e >> error errorstring
            Right a -> return a

case_getAccountList :: ExchangeConf -> IO [Account]
case_getAccountList conf = onSuccess conf getAccountList "Failed to get account list"

case_getAccount :: ExchangeConf -> AccountId -> IO Account
case_getAccount conf acID = onSuccess conf (getAccount acID) "Failed to get account info"

case_getAccountLedger :: ExchangeConf -> AccountId -> IO [Entry]
case_getAccountLedger conf acID = onSuccess conf (getAccountLedger acID) "Failed to get accountledger"

case_placeOrder :: ExchangeConf -> NewOrder -> IO OrderId
case_placeOrder conf o = onSuccess conf (createOrder o) "Failed to create order"

case_getOrder :: ExchangeConf -> OrderId -> IO Order
case_getOrder conf oid = onSuccess conf (getOrder oid) "Failed to get order info"

case_getOrderList :: ExchangeConf -> [OrderStatus] -> IO [Order]
case_getOrderList conf ss = onSuccess conf (getOrderList ss) "Failed to get order list"

case_cancelOrder :: ExchangeConf -> OrderId -> IO ()
case_cancelOrder conf oid = onSuccess conf (cancelOrder oid) "Failed to cancel order"

case_getFills :: ExchangeConf -> Maybe OrderId -> Maybe ProductId -> IO [Fill]
case_getFills conf moid mpid = onSuccess conf (getFills moid mpid) "Failed to get fills"
