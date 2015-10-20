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
-- ** If no data is received, parsing succeeds and, therefore, the parsing tests succeed**
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
        , testCase "Parse Websocket Stream"     (parseSocket conf (threadDelay $ 1000000 * 20))
        , testCase "Parse with Market Order" (parseSocket conf (do
                                                                    threadDelay $ 1000000 * 15
                                                                    oid'<- P.run_placeOrder conf P.giveAwayOrder -- place market order
                                                                    print oid'
                                                                    threadDelay $ 1000000 * 15
                                                               ))
        ]



receiveSocket :: ExchangeConf -> IO ()
receiveSocket conf = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    putStr "Connected. "
    sequence_ $ replicate 20 (receiveAndDecode conn)

receiveAndDecode :: WS.Connection -> IO ()
receiveAndDecode conn = do
    ds <- WS.receiveData conn
    let res = eitherDecode $ trace (show ds) ds
    case res :: Either String ExchangeMessage of
        Left er -> print er   >> assertFailure "Parsing failure found"
        Right v -> {-putStr "." >> -} return ()


-- Success: no parse errors   found while running
-- Failure: a parse error is  found while running
parseSocket :: ExchangeConf -> IO a -> IO ()
parseSocket conf challenge = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    putStr "Connected. "
    waitCancelThreads challenge (forever $ receiveAndDecode conn)
    return ()

waitCancelThreads :: IO a -> IO b -> IO (Either a b)
waitCancelThreads action1 action2 = do
    a <- async action1
    b <- async action2
    c <- waitEither a b
    case c of
        Left  a -> cancel b
        Right b -> cancel a
    return c
