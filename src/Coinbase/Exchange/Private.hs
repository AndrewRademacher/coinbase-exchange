{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.List
import qualified Data.Text                       as T
import           Data.UUID

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

-- Accounts

getAccountList :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => m [Account]
getAccountList = coinbaseRequest True "GET" "/accounts" voidBody

getAccount :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
           => AccountId -> m Account
getAccount (AccountId i) = coinbaseRequest True "GET" ("/accounts/" ++ toString i) voidBody

getAccountLedger :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => AccountId -> m [Entry]
getAccountLedger (AccountId i) = coinbaseRequest True "GET" ("/accounts/" ++ toString i ++ "/ledger") voidBody

getAccountHolds :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => AccountId -> m [Hold]
getAccountHolds (AccountId i) = coinbaseRequest True "GET" ("/accounts/" ++ toString i ++ "/holds") voidBody

-- Orders

createOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => NewOrder -> m OrderId
createOrder = liftM ocId . coinbaseRequest True "POST" "/orders" . Just

cancelOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => OrderId -> m ()
cancelOrder (OrderId o) = coinbaseRequest True "DELETE" ("/orders/" ++ toString o) voidBody

getOrderList :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => [OrderStatus] -> m [Order]
getOrderList os = coinbaseRequest True "GET" ("/orders?" ++ query os) voidBody
    where query [] = "status=all"
          query xs = intercalate "&" $ map (\x -> "status=" ++ map toLower (show x)) xs

getOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => OrderId -> m Order
getOrder (OrderId o) = coinbaseRequest True "GET" ("/orders/" ++ toString o) voidBody

-- Fills

getFills :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => Maybe OrderId -> Maybe ProductId -> m [Fill]
getFills moid mpid = coinbaseRequest True "GET" ("/fills?" ++ oid ++ "&" ++ pid) voidBody
    where oid = case moid of Just  v -> "order_id=" ++ toString (unOrderId v)
                             Nothing -> "order_id=all"
          pid = case mpid of Just  v -> "product_id=" ++ T.unpack (unProductId v)
                             Nothing -> "product_id=all"
