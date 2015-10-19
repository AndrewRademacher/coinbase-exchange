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


tests :: ExchangeConf -> TestTree
tests conf = testGroup "Socket"
        [ testCase "Parse Socket Stream"     (parseSocket conf (threadDelay $ 1000000 * 30))
        , testCase "Parse with Market Order" (parseSocket conf (do
                                                                    threadDelay $ 1000000 * 15
                                                                    oid'<- P.run_placeOrder conf P.giveAwayOrder -- place market order
                                                                    print oid'
                                                                    threadDelay $ 1000000 * 15
                                                               ))
        ]


-- Success: no parse errors   found while running
-- Failure: a parse error is  found while running
parseSocket :: ExchangeConf -> IO a -> IO ()
parseSocket conf challenge = subscribe (apiType conf) (ProductId "BTC-USD") $ \conn -> do
    putStr "Connected. "
    waitCancelThreads challenge $
        forever $ do
            ds <- WS.receiveData conn
            let res = eitherDecode ds
            case res :: Either String ExchangeMessage of
                Left er -> print er   >> assertFailure "Parsing failure found"
                Right v -> putStr "." >> return ()
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
