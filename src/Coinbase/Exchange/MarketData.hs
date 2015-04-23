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
import           Data.Int
import           Data.Scientific
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.UUID
import           Data.UUID.Aeson              ()
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

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
