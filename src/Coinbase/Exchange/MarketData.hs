{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Coinbase.Exchange.MarketData where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Time
import           GHC.Generics

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types

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
