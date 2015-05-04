{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
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
