{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.Socket                 (withSocketsDo)
import qualified Network.WebSockets             as WS
import           System.Locale
import           Wuss

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket

btc :: ProductId
btc = "BTC-USD"

start :: Maybe UTCTime
start = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-12T20:22:37+0000"

end :: Maybe UTCTime
end = Just $ readTime defaultTimeLocale "%FT%X%z" "2015-04-23T20:22:37+0000"

withCoinbase :: Exchange a -> IO a
withCoinbase act = do
        mgr <- newManager tlsManagerSettings
        res <- runExchange (ExchangeConf mgr) act
        case res of
            Right s -> return s
            Left  f -> error $ show f

-- Socket Experimientation

app :: WS.ClientApp ()
app conn = do
        putStrLn "Connected"
        _ <- forkIO $ forever $ do
            ds <- WS.receiveData conn
            let res = eitherDecode ds
            print (res :: Either String ExchangeMessage)
            -- print (decode ds :: Maybe ExchangeMessage)
            -- msg <- WS.receiveData conn
            -- liftIO $ T.putStrLn msg

        WS.sendBinaryData conn $ encode (Subscribe btc)
        _ <- forever $ threadDelay (1000000 * 60)
        return ()


-- app :: WS.ClientApp ()
-- app conn = do
--         putStrLn "Connected"
--
--         _ <- forkIO $ forever $ do
--             msg <- WS.receiveData conn
--             liftIO $ T.putStrLn msg
--
--         let loop = do
--                 line <- T.getLine
--                 unless (T.null line) $ WS.sendTextData conn line >> loop
--
--         loop
--         WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = withSocketsDo $
    runSecureClient "ws-feed.exchange.coinbase.com" 443 "/" app
