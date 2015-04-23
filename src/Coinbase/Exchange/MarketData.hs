{-# LANGUAGE DeriveGeneric #-}

module Coinbase.Exchange.MarketData where

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

getExchangeTime :: Exchange ExchangeTime
getExchangeTime = coinbaseRequest liveRest "/time"
