module Main where

import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           Test.Tasty

import           Coinbase.Exchange.Types

import qualified Coinbase.Exchange.MarketData.Test as MarketData

main :: IO ()
main = do
        mgr <- newManager tlsManagerSettings
        defaultMain (tests $ ExchangeConf mgr)

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Tests"
        [ MarketData.tests conf
        ]
