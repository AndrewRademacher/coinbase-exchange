{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private
    ( getAccountList
    , getAccount
    , getAccountLedger
    , getAccountHolds

    , createOrder
    , cancelOrder
    , getOrderList
    , getOrder

    , getFills

    , createTransfer

    , getRealCoinbaseAccountList
    , sendBitcoins

    , module Coinbase.Exchange.Types.Private
    ) where

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
getAccountList = coinbaseGet True "/accounts" voidBody

getAccount :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
           => AccountId -> m Account
getAccount (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i) voidBody

getAccountLedger :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => AccountId -> m [Entry]
getAccountLedger (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i ++ "/ledger") voidBody

getAccountHolds :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => AccountId -> m [Hold]
getAccountHolds (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i ++ "/holds") voidBody

-- Orders

createOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => NewOrder -> m OrderId
createOrder = liftM ocId . coinbasePost True "/orders" . Just

cancelOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => OrderId -> m ()
cancelOrder (OrderId o) = coinbaseDelete True ("/orders/" ++ toString o) voidBody

getOrderList :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => [OrderStatus] -> m [Order]
getOrderList os = coinbaseGet True ("/orders?" ++ query os) voidBody
    where query [] = "status=all"
          query xs = intercalate "&" $ map (\x -> "status=" ++ map toLower (show x)) xs

getOrder :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => OrderId -> m Order
getOrder (OrderId o) = coinbaseGet True ("/orders/" ++ toString o) voidBody

-- Fills

getFills :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => Maybe OrderId -> Maybe ProductId -> m [Fill]
getFills moid mpid = coinbaseGet True ("/fills?" ++ oid ++ "&" ++ pid) voidBody
    where oid = case moid of Just  v -> "order_id=" ++ toString (unOrderId v)
                             Nothing -> ""
          pid = case mpid of Just  v -> "product_id=" ++ T.unpack (unProductId v)
                             Nothing -> ""

-- Transfers

createTransfer :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => TransferToCoinbase -> m TransferToCoinbaseResponse
createTransfer = coinbasePost True "/transfers" . Just

getRealCoinbaseAccountList :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => m String
getRealCoinbaseAccountList = realCoinbaseGet True "/v2/accounts" voidBody -- FIX ME! This is TO DO.


sendBitcoins :: (MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => CoinbaseAccountId -> BTCTransferReq -> m BTCTransferResponse
sendBitcoins accountId = realCoinbasePost True ("/v2/accounts/" ++ show (unCoinbaseAccountId accountId) ++ "/transactions")  . Just
