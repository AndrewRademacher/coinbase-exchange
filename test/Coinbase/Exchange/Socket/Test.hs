{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Socket.Test (tests) where

import           Data.Aeson
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.Async

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Network.WebSockets              as WS

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket

import qualified Coinbase.Exchange.Private.Test  as P

import Debug.Trace

-------------------------------------
-- NOTE: [Connectivity Precondition]
--
-- The parsing tests are time-based and assume we are receiving messages during the
-- time we are connected. However, those tests are NOT FAILSAFE.
--
-- ** If no data is received, parsing succeeds and, therefore, the parsing tests succeed **
--
-- To ensure this unsafe behavior does not go unnoticed (we thinking we are
-- parsing correctly when, in fact, we are not parsing anything at all),
-- We first verify we can receive at least 20 messages (i.e. a fixed minimum number)
-- from the socket, before running the parsing tests.
-------------------------------------


tests :: ExchangeConf -> TestTree
tests conf = testGroup "Socket"
        -- See NOTE: [Connectivity Precondition]
        [ testCase "Do I receive messages?"  (receiveSocket conf)
        , testCase "Parse Websocket Stream"  (parseSocket conf (threadDelay $ 1000000 * 20))
        , testCase "Parse with Market Order" (parseSocket conf (do
                                                                    threadDelay $ 1000000 * 15
                                                                    oid'<- P.run_placeOrder conf P.giveAwayOrder -- place market order
                                                                    threadDelay $ 1000000 * 15
                                                               ))
        , testCase "Decode Re-Encode Decode" (reencodeSocket conf)
        ]



receiveSocket :: ExchangeConf -> IO ()
receiveSocket conf = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    sequence_ $ replicate 20 (receiveAndDecode conn)

-- Success: no parse errors   found while running
-- Failure: a parse error is  found while running
parseSocket :: ExchangeConf -> IO a -> IO ()
parseSocket conf challenge = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    waitCancelThreads challenge (forever $ receiveAndDecode conn)
    return ()


-- FIX ME! there's no guarantee we are hitting all order types.
-- a more thorough test would be better.
reencodeSocket :: ExchangeConf -> IO ()
reencodeSocket conf = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    sequence_ $ replicate 1000 (decodeEncode conn) -- currently takes under a minute on 'Live' feed

decodeEncode :: WS.Connection -> IO ()
decodeEncode conn = do
    ds <- WS.receiveData conn
    let res = eitherDecode ds
    case res :: Either String ExchangeMessage of
        Left er -> assertFailure "Failure parsing data from exchange" >> print er
        Right received -> do
            let enc = encode received
                dec = eitherDecode enc
            if dec == res
                then return ()
                else do
                    putStrLn $ "### original: " ++ show res
                    putStrLn $ "### obtained: " ++ show dec
                    assertFailure "decoded object is different from original"



receiveAndDecode :: WS.Connection -> IO ()
receiveAndDecode conn = do
    ds <- WS.receiveData conn
    let res = eitherDecode {- $ trace (show ds) -} ds
    case res :: Either String ExchangeMessage of
        Left er -> print er   >> assertFailure "Parsing failure found"
        Right v -> return ()

waitCancelThreads :: IO a -> IO b -> IO (Either a b)
waitCancelThreads action1 action2 = do
    a <- async action1
    b <- async action2
    c <- waitEither a b
    case c of
        Left  a -> cancel b
        Right b -> cancel a
    return c
