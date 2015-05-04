{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8           as CBS
import           Data.Maybe
import           Data.Time
import           Data.UUID
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment
import           System.Locale

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Types.Socket

main :: IO ()
main = putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

accountId :: AccountId
accountId = AccountId $ fromJust $ fromString "52072cbb-4e76-496f-a479-166cb4d177fa"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr     <- newManager tlsManagerSettings
        tKey    <- liftM CBS.pack $ getEnv "COINBASE_KEY"
        tSecret <- liftM CBS.pack $ getEnv "COINBASE_SECRET"
        tPass   <- liftM CBS.pack $ getEnv "COINBASE_PASSPHRASE"

        case mkToken tKey tSecret tPass of
            Right tok -> do res <- runExchange (ExchangeConf mgr (Just tok) Live) act
                            case res of
                                Right s -> return s
                                Left  f -> error $ show f
            Left   er -> error $ show er
