{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8           as CBS
import           Data.Maybe
import           Data.Time
import           Data.UUID
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.WebSockets              as WS
import           System.Environment

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Types.Socket

main :: IO ()
main = printSocket -- putStrLn "Use GHCi."

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ parseTimeOrError True defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr     <- newManager tlsManagerSettings
        tKey    <- liftM CBS.pack $ getEnv "COINBASE_KEY"
        tSecret <- liftM CBS.pack $ getEnv "COINBASE_SECRET"
        tPass   <- liftM CBS.pack $ getEnv "COINBASE_PASSPHRASE"

        sbox    <- getEnv "COINBASE_SANDBOX"
        let apiType  = case sbox of
                        "FALSE" -> Live
                        "TRUE"  -> Sandbox
                        _       -> error "Coinbase sandbox option must be either: TRUE or FALSE (all caps)"

        case mkToken tKey tSecret tPass of
            Right tok -> do res <- runExchange (ExchangeConf mgr (Just tok) apiType) act
                            case res of
                                Right s -> return s
                                Left  f -> error $ show f
            Left   er -> error $ show er

printSocket :: IO ()
printSocket = subscribe Live btc $ \conn -> do
        putStrLn "Connected."
        _ <- forkIO $ forever $ do
            ds <- WS.receiveData conn
            let res = eitherDecode ds
            case res :: Either String ExchangeMessage of
                Left er -> print er
                Right v -> print v
        _ <- forever $ threadDelay (1000000 * 60)
        return ()
