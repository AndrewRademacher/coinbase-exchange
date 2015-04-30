{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Coinbase.Exchange.MarketData where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.List
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID.Aeson                    ()

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                   (defaultTimeLocale)
#else
import           System.Locale                      (defaultTimeLocale)
#endif

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.MarketData

-- Products

getProducts :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => m [Product]
getProducts = coinbaseRequest liveRest "/products"

-- Order Book

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

getProductTicker :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => ProductId -> m Tick
getProductTicker (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/ticker")

-- Product Trades

-- | Currently Broken: coinbase api doesn't return valid ISO 8601 dates for this route.
getTrades :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
          => ProductId -> m [Trade]
getTrades (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/trades")

-- Historic Rates (Candles)

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

getStats :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => ProductId -> m Stats
getStats (ProductId p) = coinbaseRequest liveRest ("/products/" ++ T.unpack p ++ "/stats")

-- Exchange Currencies

getCurrencies :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
              => m [Currency]
getCurrencies = coinbaseRequest liveRest "/currencies"

-- Exchange Time

getExchangeTime :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => m ExchangeTime
getExchangeTime = coinbaseRequest liveRest "/time"
