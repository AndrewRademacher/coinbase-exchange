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
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Scientific
import           Data.String
import           Data.Text                    (Text)
import           Data.Time
import           GHC.Generics

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

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
