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
getProducts = coinbaseRequest False "GET" "/products" voidBody

-- Order Book

getTopOfBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ProductId -> m (Book Aggregate)
getTopOfBook (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/book?level=1") voidBody

getTop50OfBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => ProductId -> m (Book Aggregate)
getTop50OfBook (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/book?level=2") voidBody

getOrderBook :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ProductId -> m (Book OrderId)
getOrderBook (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/book?level=3") voidBody

-- Product Ticker

getProductTicker :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => ProductId -> m Tick
getProductTicker (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/ticker") voidBody

-- Product Trades

-- | Currently Broken: coinbase api doesn't return valid ISO 8601 dates for this route.
getTrades :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
          => ProductId -> m [Trade]
getTrades (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/trades") voidBody

-- Historic Rates (Candles)

type StartTime  = UTCTime
type EndTime    = UTCTime
type Scale      = Int

getHistory :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
           => ProductId -> Maybe StartTime -> Maybe EndTime -> Maybe Scale -> m [Candle]
getHistory (ProductId p) start end scale = coinbaseRequest False "GET" path voidBody
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
getStats (ProductId p) = coinbaseRequest False "GET" ("/products/" ++ T.unpack p ++ "/stats") voidBody

-- Exchange Currencies

getCurrencies :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
              => m [Currency]
getCurrencies = coinbaseRequest False "GET" "/currencies" voidBody

-- Exchange Time

getExchangeTime :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => m ExchangeTime
getExchangeTime = coinbaseRequest False "GET" "/time" voidBody
