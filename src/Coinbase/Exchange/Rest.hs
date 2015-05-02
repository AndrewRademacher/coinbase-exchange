{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Rest
    ( coinbaseRequest
    , voidBody
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Coinbase.Exchange.Types

type Signed = Bool

voidBody :: Maybe ()
voidBody = Nothing

coinbaseRequest :: ( ToJSON a
                   , FromJSON b
                   , MonadResource m
                   , MonadReader ExchangeConf m
                   , MonadError ExchangeFailure m )
                => Signed -> Method -> Endpoint -> Path -> Maybe a -> m b
coinbaseRequest sgn meth e p ma = do
        conf <- ask
        req  <- parseUrl $ e ++ p
        let req' = req { method         = meth
                       , requestHeaders = [ ("user-agent", "haskell") ]
                       }

        res <- flip http (manager conf) $ signMessage sgn . encodeBody ma $ req'
        processResponse res

encodeBody :: (ToJSON a) => Maybe a -> Request -> Request
encodeBody (Just a) req = req { requestBody = RequestBodyBS $ LBS.toStrict $ encode a}
encodeBody Nothing  req = req

signMessage :: Signed -> Request -> Request
signMessage True  req = undefined
signMessage False req = req

processResponse :: (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => Response (ResumableSource m BS.ByteString) -> m b
processResponse res =
    case responseStatus res of
        s | s == status200 -> do body <- responseBody res $$+- sinkParser (fmap fromJSON json)
                                 case body of
                                     Success b -> return b
                                     Error  er -> throwError $ ParseFailure $ T.pack er
          | otherwise      -> do body <- responseBody res $$+- CB.sinkLbs
                                 throwError $ ApiFailure $ TE.decodeUtf8 $ LBS.toStrict body
