{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Rest where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           Coinbase.Exchange.Types

coinbaseRequest :: (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => Endpoint -> Path -> m b
coinbaseRequest e p = do
        req <- parseUrl $ e ++ p
        let req' = req { requestHeaders = [ ("user-agent", "haskell") ]
                       }

        res <- http req' =<< getManager
        case responseStatus res of
            s | s == status200 -> do body <- responseBody res $$+- sinkParser (fmap fromJSON json)
                                     case body of
                                         Success b -> return b
                                         Error  er -> throwError $ ParseFailure $ T.pack er
              | otherwise      -> do body <- responseBody res $$+- CB.sinkLbs
                                     throwError $ ApiFailure $ TE.decodeUtf8 $ LBS.toStrict body
