{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Private.Test
    ( tests
    ) where

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import           Data.Time
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

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Private"
        [ case_parse conf "getAccountList" $ getAccountList >>= liftIO . writeIORef accountId . Just . accId . head
        , case_parse conf "getAccount" $ liftIO (readIORef accountId) >>= getAccount . fromJust
        , case_parse conf "getAccountLedger" $ liftIO (readIORef accountId) >>= getAccountLedger . fromJust
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
