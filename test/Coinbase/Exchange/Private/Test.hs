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
import           Test.Tasty
import           Test.Tasty.HUnit

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

-- accountId :: IORef (Maybe AccountId)
-- accountId = unsafePerformIO $ newIORef Nothing
-- {-# NOINLINE accountId #-}
--
-- cancelOrderId :: IORef (Maybe OrderId)
-- cancelOrderId = unsafePerformIO $ newIORef Nothing
-- {-# NOINLINE cancelOrderId #-}
--
-- tests conf = testGroup "Private"
--         [ case_parse conf "getAccountList"   $ getAccountList >>= liftIO . writeIORef accountId . Just . accId . head
--         , case_parse conf "getAccount"       $ liftIO (readIORef accountId) >>= getAccount . fromJust
--         , case_parse conf "getAccountLedger" $ liftIO (readIORef accountId) >>= getAccountLedger . fromJust
--         , case_placeOrder conf
--         , case_parse conf "getOrderList"     $ getOrderList [Open, Pending]
--         , case_parse conf "getOrder"         $ liftIO (readIORef cancelOrderId) >>= getOrder . fromJust
--         , case_cancelOrder conf
--         , case_parse conf "getFills"         $ getFills Nothing Nothing
--         ]


tests :: ExchangeConf -> TestTree
tests conf = testGroup "Private"
        [ testCase "getAccountList" (do case_getAccountList conf
                                        return ()
                                    )
        , testCase "getUSDAccount"  (do as <- case_getAccountList conf
                                        let usdAccount = case filter (\a -> accCurrency a == CurrencyId "USD") as of
                                                            [a] -> a
                                                            []  -> error "no USD denominated account found"
                                                            _   -> error "more than one account denominated in USD found"
                                        mAc <- case_getAccount conf (accId usdAccount)
                                        case mAc of
                                            Nothing -> assertFailure "No account info returned"
                                            Just ac -> assertEqual "accounts match" usdAccount ac
                                    )
        -- , case_parse conf "getAccount"       $ liftIO (readIORef accountId) >>= getAccount . fromJust
        -- , case_parse conf "getAccountLedger" $ liftIO (readIORef accountId) >>= getAccountLedger . fromJust
        -- , case_placeOrder conf
        -- , case_parse conf "getOrderList"     $ getOrderList [Open, Pending]
        -- , case_parse conf "getOrder"         $ liftIO (readIORef cancelOrderId) >>= getOrder . fromJust
        -- , case_cancelOrder conf
        -- , case_parse conf "getFills"         $ getFills Nothing Nothing
        ]

case_getAccountList :: ExchangeConf -> IO [Account]
case_getAccountList conf = do
        res <- liftIO $ runExchange conf getAccountList
        print res
        case res of
            Left  e  -> do print e >> assertFailure "Failed to get account list" >> return []
            Right as -> case as of
                        [] -> assertFailure "Received empty list of accounts"  >> return []
                        _  -> return as

case_getAccount :: ExchangeConf -> AccountId -> IO (Maybe Account)
case_getAccount conf acID = do
        res <- liftIO $ runExchange conf (getAccount acID)
        print res
        case res of Left  e -> print e >> assertFailure "Failed to get account info" >> return Nothing
                    Right a -> return $ Just a

-- case_run :: Show a => ExchangeConf -> String -> Exchange a -> TestTree
-- case_run conf l fn = testCase l $ do
--         v <- liftIO $ runExchange conf fn
--
--         case v of Left  e -> print e
--                   Right _ -> return ()
--         assertBool "Failed to parse."
--             (case v of
--                 Left  _ -> False
--                 Right _ -> True)




--
-- case_placeOrder :: ExchangeConf -> TestTree
-- case_placeOrder conf = testCase "place an order" $ do
--         res <- liftIO $ runExchange conf $
--             createOrder NewOrder
--                 { noSize      = 1
--                 , noPrice     = 1
--                 , noSide      = Buy
--                 , noProductId = "BTC-USD"
--                 , noClientOid = Nothing
--                 , noSelfTrade = Nothing
--                 }
--         case res of
--             Left  e -> print e >> assertFailure "Unable to place order."
--             Right v -> writeIORef cancelOrderId (Just v)
--
-- case_cancelOrder :: ExchangeConf -> TestTree
-- case_cancelOrder conf = testCase "cancel an order" $ do
--         oid <- liftM fromJust $ readIORef cancelOrderId
--         res <- liftIO $ runExchange conf $ cancelOrder oid
--         case res of
--             Left  e -> print e >> assertFailure "Unable to cancel order."
--             Right _ -> return ()
