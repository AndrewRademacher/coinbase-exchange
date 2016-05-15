{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types
    ( ApiType (..)
    , Endpoint
    , Path

    , website
    , sandboxRest
    , sandboxSocket
    , liveRest
    , liveSocket
    , liveRealCoinbaseRest
    , sandboxRealCoinbaseRest

    , Key
    , Secret
    , Passphrase

    , Token
    , key
    , secret
    , passphrase
    , mkToken

    , ExchangeConf (..)
    , ExchangeFailure (..)

    , Exchange
    , ExceptT
    , runExchange
    , runExchangeT
    , execExchange
    , execExchangeT

    , getManager
    ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString
import qualified Data.ByteString.Base64       as Base64
import           Data.Data
import           Data.Text                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit

-- API URLs

data ApiType
    = Sandbox
    | Live
    deriving (Show)

type Endpoint = String
type Path     = String

website :: Endpoint
website = "https://public.sandbox.exchange.coinbase.com"

sandboxRest :: Endpoint
sandboxRest = "https://api-public.sandbox.exchange.coinbase.com"

sandboxSocket :: Endpoint
sandboxSocket = "ws-feed-public.sandbox.exchange.coinbase.com"

liveRest :: Endpoint
liveRest = "https://api.exchange.coinbase.com"

liveSocket :: Endpoint
liveSocket = "ws-feed.exchange.coinbase.com"

-- Coinbase needs to provide real BTC transfers through the exchange API soon,
-- making 2 API calls with 2 sets of authentication credentials is ridiculous.
liveRealCoinbaseRest :: Endpoint
liveRealCoinbaseRest = "https://api.coinbase.com"

sandboxRealCoinbaseRest :: Endpoint
sandboxRealCoinbaseRest = "https://api.sandbox.coinbase.com"

-- Monad Stack

type Key        = ByteString
type Secret     = ByteString
type Passphrase = ByteString

data Token
    = Token
        { key        :: ByteString
        , secret     :: ByteString
        , passphrase :: ByteString
        }

mkToken :: Key -> Secret -> Passphrase -> Either String Token
mkToken k s p = case Base64.decode s of
                    Right s' -> Right $ Token k s' p
                    Left  e  -> Left e

data ExchangeConf
    = ExchangeConf
        { manager   :: Manager
        , authToken :: Maybe Token
        , apiType   :: ApiType
        }

data ExchangeFailure = ParseFailure Text
                     | ApiFailure Text
                     | AuthenticationRequiredFailure Text
                     | AuthenticationRequiresByteStrings
                     deriving (Show, Data, Typeable, Generic)

instance Exception ExchangeFailure

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

execExchange :: ExchangeConf -> Exchange a -> IO a
execExchange = execExchangeT

execExchangeT :: (MonadThrow m, MonadBaseControl IO m) => ExchangeConf -> ExchangeT m a -> m a
execExchangeT conf act = do
    v <- runExceptT . flip runReaderT conf . runResourceT . unExchangeT $ act
    case v of
        Left er -> throwM er
        Right v -> return v

-- Utils

getManager :: (MonadReader ExchangeConf m) => m Manager
getManager = do
        conf <- ask
        return $ manager conf
