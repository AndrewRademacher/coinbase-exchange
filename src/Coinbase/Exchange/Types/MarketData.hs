{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Coinbase.Exchange.Types.MarketData where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Int
import           Data.Scientific
import           Data.String
import           Data.Text                    (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.UUID.Aeson              ()
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

import           Coinbase.Exchange.Types.Core

-- Products

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

-- Product Trades

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

-- Exchange Time

data ExchangeTime
    = ExchangeTime
        { timeIso   :: UTCTime
        , timeEpoch :: Double
        }
    deriving (Show, Generic)

instance FromJSON ExchangeTime where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
