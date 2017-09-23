{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.MarketData where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Except
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Data
import           Data.Hashable
import           Data.Int
import           Data.String
import           Data.Text                    (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.UUID.Aeson              ()
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

import           Coinbase.Exchange.Types.Core hiding (OrderStatus (..))

-- Products

data Product
    = Product
        { prodId             :: ProductId
        , prodBaseCurrency   :: CurrencyId
        , prodQuoteCurrency  :: CurrencyId
        , prodBaseMinSize    :: CoinScientific
        , prodBaseMaxSize    :: CoinScientific
        , prodQuoteIncrement :: CoinScientific
        , prodDisplayName    :: Text
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Product
instance ToJSON Product where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Product where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Order Book

data Book a
    = Book
        { bookSequence :: Sequence
        , bookBids     :: [BookItem a]
        , bookAsks     :: [BookItem a]
        }
    deriving (Show, Data, Typeable, Generic)

instance (NFData a) => NFData (Book a)
instance (ToJSON a) => ToJSON (Book a) where
    toJSON = genericToJSON coinbaseAesonOptions
instance (FromJSON a) => FromJSON (Book a) where
    parseJSON = genericParseJSON coinbaseAesonOptions

data BookItem a = BookItem Price Size a
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance (NFData a) => NFData (BookItem a)
instance (ToJSON a) => ToJSON (BookItem a) where
    toJSON = genericToJSON defaultOptions
instance (FromJSON a) => FromJSON (BookItem a) where
    parseJSON = genericParseJSON defaultOptions

-- Product Ticker

data Tick
    = Tick
        { tickTradeId :: Word64
        , tickPrice   :: Price
        , tickSize    :: Size
        , tickTime    :: Maybe UTCTime
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Tick
instance ToJSON Tick where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Tick where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Product Trades

data Trade
    = Trade
        { tradeTime    :: UTCTime
        , tradeTradeId :: TradeId
        , tradePrice   :: Price
        , tradeSize    :: Size
        , tradeSide    :: Side
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Trade
instance ToJSON Trade where
    toJSON Trade{..} = object [ "time"      .= tradeTime
                              , "trade_id"  .= tradeTradeId
                              , "price"     .= tradePrice
                              , "size"      .= tradeSize
                              , "side"      .= tradeSide
                              ]
instance FromJSON Trade where
    parseJSON (Object m) = Trade <$> m .: "time"
                                 <*> m .: "trade_id"
                                 <*> m .: "price"
                                 <*> m .: "size"
                                 <*> m .: "side"
    parseJSON _ = mzero

-- Historic Rates (Candles)

data Candle = Candle UTCTime Low High Open Close Volume
    deriving (Show, Data, Typeable, Generic)

instance NFData Candle
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
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype High = High { unHigh :: Double }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Open = Open { unOpen :: Double }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Close = Close { unClose :: Double }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Volume = Volume { unVolume :: Double }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

-- Product Stats

data Stats
    = Stats
        { statsOpen   :: Open
        , statsHigh   :: High
        , statsLow    :: Low
        , statsVolume :: Volume
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Stats
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
    deriving (Show, Data, Typeable, Generic)

instance NFData Currency
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
    deriving (Show, Data, Typeable, Generic)

instance ToJSON ExchangeTime where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON ExchangeTime where
    parseJSON = genericParseJSON coinbaseAesonOptions
