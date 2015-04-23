{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Conduit
import qualified Data.Conduit.Binary     as BIN
import qualified Data.Conduit.List       as CL
import           Network.HTTP.Conduit
import           System.IO

import           Coinbase.Exchange.Types

main :: IO ()
main = do
        req <- parseUrl $ "https://api.exchange.coinbase.com/time"
        let req' = req { requestHeaders = [ ("user-agent", "Haskell") ]
                       }

        withManager $ \manager -> do
            res <- http req' manager
            responseBody res $$+- BIN.sinkHandle stdout
