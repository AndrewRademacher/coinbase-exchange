module Coinbase.Exchange.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

data ExchangeConf = ExchangeConf

data ExchangeFailure = ExchangeFailure

newtype ExchangeT m a = ExchangeT { unExchangeT :: ResourceT (ReaderT ExchangeConf (ExceptT ExchangeFailure m)) a }
