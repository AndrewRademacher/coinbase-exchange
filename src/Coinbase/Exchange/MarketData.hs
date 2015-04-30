{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Coinbase.Exchange.MarketData where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Scientific
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.UUID
import           Data.UUID.Aeson              ()
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format             (defaultTimeLocale)
#else
import           System.Locale                (defaultTimeLocale)
#endif

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

-- Products

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

data Product
    = Product
        { prodId             :: ProductId
        , prodBaseCurrency   :: CurrencyId
        , prodQuoteCurrency  :: CurrencyId
        , prodBaseMinSize    :: Scientific
        , prodBaseMaxSize    :: Scientific
        , prodQuoteIncrement :: Scientific
        , prodDisplayName    :: Text
        }
    deriving (Show, Generic)

instance FromJSON Product where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

getProducts :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => m [Product]
getProducts = coinbaseRequest liveRest "/products"

-- Order Book

data Book a
    = Book
        { bookSequence :: Word64
        , bookBids     :: [Bid a]
        , bookAsks     :: [Ask a]
        }
    deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Book a) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Ask a = Ask Price Size a
    deriving (Eq, Ord, Show, Read, Generic)

instance (FromJSON a) => FromJSON (Ask a) where
    parseJSON (Array v)
        = case V.length v of
            3 -> Ask <$> liftM (Price . read) (parseJSON (v V.! 0))
                     <*> liftM (Size . read) (parseJSON (v V.! 1))
                     <*> parseJSON (v V.! 2)
            _ -> mzero
    parseJSON _ = mzero

data Bid a = Bid Price Size a
    deriving (Eq, Ord, Show, Read, Generic)

instance (FromJSON a) => FromJSON (Bid a) where
    parseJSON (Array v)
        = case V.length v of
            3 -> Bid <$> liftM (Price . read) (parseJSON (v V.! 0))
                     <*> liftM (Size . read) (parseJSON (v V.! 1))
                     <*> parseJSON (v V.! 2)
            _ -> mzero
    parseJSON _ = mzero

newtype Price = Price { unPrice :: Scientific }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Size = Size { unSize :: Scientific }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype OrderId = OrderId { unOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Aggregate = Aggregate { unAggregate :: Int64 }
    deriving (Eq, Ord, Show, Read, Num, FromJSON, ToJSON)

getTopOfBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ProductId -> m (Book Aggregate)
getTopOfBook (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/book?level=1")

getTop50OfBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => ProductId -> m (Book Aggregate)
getTop50OfBook (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/book?level=2")

getOrderBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ProductId -> m (Book OrderId)
getOrderBook (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/book?level=3")

-- Product Ticker

data Tick
    = Tick
        { tickTradeId :: Word64
        , tickPrice   :: Price
        , tickSize    :: Size
        , tickTime    :: UTCTime
        }
    deriving (Show, Generic)

instance FromJSON Tick where
    parseJSON (Object m)
        = Tick <$> m .: "trade_id"
               <*> liftM (Price . read) (m .: "price")
               <*> liftM (Size . read) (m .: "size")
               <*> m .: "time"
    parseJSON _ = mzero

getProductTicker :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => ProductId -> m Tick
getProductTicker (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/ticker")

-- Product Trades

newtype TradeId = TradeId { unTradeId :: Word64 }
    deriving (Eq, Ord, Show, Read, Num, FromJSON, ToJSON)

data Trade
    = Trade
        { tradeTime  :: UTCTime
        , tradeId    :: TradeId
        , tradePrice :: Price
        , tradeSize  :: Size
        , tradeSide  :: Side
        }
    deriving (Show, Generic)

instance FromJSON Trade where
    parseJSON (Object m)
        = Trade <$> m .: "time"
                <*> m .: "trade_id"
                <*> liftM (Price . read) (m .: "price")
                <*> liftM (Size . read) (m .: "size")
                <*> m .: "side"
    parseJSON _ = mzero

data Side = Buy | Sell
    deriving (Eq, Show, Read, Generic)

instance ToJSON Side where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }

instance FromJSON Side where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

-- | Currently Broken: coinbase api doesn't return valid ISO 8601 dates for this route.
getTrades :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
          => ProductId -> m [Trade]
getTrades (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/trades")

-- Historic Rates (Candles)

data Candle = Candle UTCTime Low High Open Close Volume
    deriving (Show, Generic)

instance FromJSON Candle where
    parseJSON (Array v)
        = case V.length v of
            6 -> Candle <$> liftM (posixSecondsToUTCTime . fromIntegral) (parseJSON (v V.! 0) :: Parser Int64)
                        <*> parseJSON (v V.! 1)
                        <*> parseJSON (v V.! 2)
                        <*> parseJSON (v V.! 3)
                        <*> parseJSON (v V.! 4)
                        <*> parseJSON (v V.! 5)
            _ -> mzero
    parseJSON _ = mzero

newtype Low = Low { unLow :: Double }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype High = High { unHigh :: Double }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Open = Open { unOpen :: Double }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Close = Close { unClose :: Double }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Volume = Volume { unVolume :: Double }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

type StartTime  = UTCTime
type EndTime    = UTCTime
type Scale      = Int

getHistory :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
           => ProductId -> Maybe StartTime -> Maybe EndTime -> Maybe Scale -> m [Candle]
getHistory (ProductId p) start end scale = coinbaseRequest liveRest path
    where path   = "/products/" ++ T.unpack p ++ "/candles?" ++ params
          params = intercalate "&" $ map (\(k, v) -> k ++ "=" ++ v) $ start' ++ end' ++ scale'
          start' = case start of Nothing -> []
                                 Just  t -> [(      "start",  fmt t)]
          end'   = case end   of Nothing -> []
                                 Just  t -> [(        "end",  fmt t)]
          scale' = case scale of Nothing -> []
                                 Just  s -> [("granularity", show s)]
          fmt t  = let t' = formatTime defaultTimeLocale "%FT%T." t
                    in t' ++ take 6 (formatTime defaultTimeLocale "%q" t) ++ "Z"

-- Product Stats

data Stats
    = Stats
        { statsOpen   :: Open
        , statsHigh   :: High
        , statsLow    :: Low
        , statsVolume :: Volume
        }
    deriving (Show, Generic)

instance FromJSON Stats where
    parseJSON (Object m)
        = Stats <$> liftM (Open . read) (m .: "open")
                <*> liftM (High . read) (m .: "high")
                <*> liftM (Low . read) (m .: "low")
                <*> liftM (Volume . read) (m .: "volume")
    parseJSON _ = mzero

getStats :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => ProductId -> m Stats
getStats (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/stats")

-- Exchange Currencies

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

data Currency
    = Currency
        { curId      :: CurrencyId
        , curName    :: Text
        , curMinSize :: Scientific
        }
    deriving (Show, Generic)

instance FromJSON Currency where
    parseJSON (Object m)
        = Currency <$> m .: "id"
                   <*> m .: "name"
                   <*> liftM read (m .: "min_size")
    parseJSON _ = mzero

getCurrencies :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
              => m [Currency]
getCurrencies = coinbaseRequest liveRest "/currencies"

-- Exchange Time

data ExchangeTime
    = ExchangeTime
        { timeIso   :: UTCTime
        , timeEpoch :: Double
        }
    deriving (Show, Generic)

instance FromJSON ExchangeTime where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

getExchangeTime :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => m ExchangeTime
getExchangeTime = coinbaseRequest liveRest "/time"
