module Coinbase.Exchange.Types where

data ExchangeConf

data ExchangeFailure

newtype ExchangeT m a = ExchangeT { unExchangeT :: ResourceT (ReaderT ExchangeConf (ExceptT ExchangeFailure m)) a }
