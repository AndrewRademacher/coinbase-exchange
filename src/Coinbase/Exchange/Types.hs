{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Text                    (Text)
import           Network.HTTP.Conduit

-- API URLs

type Endpoint = String
type Path     = String

website :: Endpoint
website = "https://public.sandbox.exchange.coinbase.com"

sandboxRest :: Endpoint
sandboxRest = "https://api-public.sandbox.exchange.coinbase.com"

sandboxSocket :: Endpoint
sandboxSocket = "wss://ws-feed-public.sandbox.exchange.coinbase.com"

liveRest :: Endpoint
liveRest = "https://api.exchange.coinbase.com"

liveSocket :: Endpoint
liveSocket = "wss://ws-feed.exchange.coinbase.com"

-- Monad Stack

data ExchangeConf
    = ExchangeConf
        { manager :: Manager
        }

data ExchangeFailure = ParseFailure Text
                     | ApiFailure Text
                     deriving (Show)

type Exchange a = ExchangeT IO a

newtype ExchangeT m a = ExchangeT { unExchangeT :: ResourceT (ReaderT ExchangeConf (ExceptT ExchangeFailure m)) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow
             , MonadError ExchangeFailure
             , MonadReader ExchangeConf
             )

deriving instance (MonadBase IO m) => MonadBase IO (ExchangeT m)
deriving instance (Monad m, MonadThrow m, MonadIO m, MonadBase IO m) => MonadResource (ExchangeT m)

runExchange :: ExchangeConf -> Exchange a -> IO (Either ExchangeFailure a)
runExchange = runExchangeT

runExchangeT :: MonadBaseControl IO m => ExchangeConf -> ExchangeT m a -> m (Either ExchangeFailure a)
runExchangeT conf = runExceptT . flip runReaderT conf . runResourceT . unExchangeT

-- Utils

getManager :: (MonadReader ExchangeConf m) => m Manager
getManager = do
        conf <- ask
        return $ manager conf
