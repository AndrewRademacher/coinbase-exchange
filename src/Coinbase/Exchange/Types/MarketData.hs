{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Order Book

data Book a
    = Book
        { bookSequence :: Word64
        , bookBids     :: [Bid a]
        , bookAsks     :: [Ask a]
        }
    deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Book a) where
    parseJSON = genericParseJSON coinbaseAesonOptions

data Ask a = Ask Price Size a
    deriving (Eq, Ord, Show, Read, Generic)

instance (ToJSON a) => ToJSON (Ask a) where
    toJSON = genericToJSON defaultOptions

instance (FromJSON a) => FromJSON (Ask a) where
    parseJSON = genericParseJSON defaultOptions

data Bid a = Bid Price Size a
    deriving (Eq, Ord, Show, Read, Generic)

instance (ToJSON a) => ToJSON (Bid a) where
    toJSON = genericToJSON defaultOptions

instance (FromJSON a) => FromJSON (Bid a) where
    parseJSON = genericParseJSON defaultOptions

-- Product Ticker

data Tick
    = Tick
        { tickTradeId :: Word64
        , tickPrice   :: Price
        , tickSize    :: Size
        , tickTime    :: UTCTime
        }
    deriving (Show, Generic)

instance ToJSON Tick where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Tick where
    parseJSON = genericParseJSON coinbaseAesonOptions

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

instance ToJSON Trade where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Trade where
    parseJSON = genericParseJSON coinbaseAesonOptions

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

instance ToJSON Stats where
    toJSON Stats{..} = object
        [ "open"    .= show statsOpen
        , "high"    .= show statsHigh
        , "low"     .= show statsLow
        , "volume"  .= show statsVolume
        ]

instance FromJSON Stats where
    parseJSON (Object m)
        = Stats <$> liftM (Open . read) (m .: "open")
                <*> liftM (High . read) (m .: "high")
                <*> liftM (Low . read) (m .: "low")
                <*> liftM (Volume . read) (m .: "volume")
    parseJSON _ = mzero

-- Exchange Currencies

data Currency
    = Currency
        { curId      :: CurrencyId
        , curName    :: Text
        , curMinSize :: CoinScientific
        }
    deriving (Show, Generic)

instance ToJSON Currency where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Currency where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Exchange Time

data ExchangeTime
    = ExchangeTime
        { timeIso   :: UTCTime
        , timeEpoch :: Double
        }
    deriving (Show, Generic)

instance FromJSON ExchangeTime where
    parseJSON = genericParseJSON coinbaseAesonOptions
