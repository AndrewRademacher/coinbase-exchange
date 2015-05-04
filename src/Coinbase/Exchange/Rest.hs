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
import           Crypto.Hash
import           Data.Aeson
import           Data.Byteable
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as CBS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import           Data.Conduit.Attoparsec      (sinkParser)
import qualified Data.Conduit.Binary          as CB
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Text.Printf

import           Coinbase.Exchange.Types

type Signed = Bool

voidBody :: Maybe ()
voidBody = Nothing

coinbaseRequest :: ( ToJSON a
                   , FromJSON b
                   , MonadResource m
                   , MonadReader ExchangeConf m
                   , MonadError ExchangeFailure m )
                => Signed -> Method -> Path -> Maybe a -> m b
coinbaseRequest sgn meth p ma = do
        conf <- ask
        req  <- case apiType conf of
                    Sandbox -> parseUrl $ sandboxRest ++ p
                    Live    -> parseUrl $ liveRest ++ p
        let req' = req { method         = meth
                       , requestHeaders = [ ("user-agent", "haskell")
                                          , ("accept", "application/json")
                                          ]
                       }

        res <- flip http (manager conf) =<< signMessage sgn meth
                                        =<< encodeBody ma req'
        processResponse res

encodeBody :: (ToJSON a, Monad m)
           => Maybe a -> Request -> m Request
encodeBody (Just a) req = return req
                            { requestHeaders = requestHeaders req ++
                                               [ ("content-type", "application/json") ]
                            , requestBody = RequestBodyBS $ LBS.toStrict $ encode a
                            }
encodeBody Nothing  req = return req

signMessage :: (MonadIO m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => Signed -> Method -> Request -> m Request
signMessage True meth req = do
        conf <- ask
        case authToken conf of
            Just tok -> do time <- liftM (realToFrac . utcTimeToPOSIXSeconds) (liftIO getCurrentTime)
                                    >>= \t -> return . CBS.pack $ printf "%.3f" (t::Double)
                           rBody <- pullBody $ requestBody req
                           let presign = CBS.concat [time, meth, path req, rBody]
                               sign    = toBytes (hmac (secret tok) presign :: HMAC SHA256)
                           return req
                                { requestBody    = RequestBodyBS rBody
                                , requestHeaders = requestHeaders req ++
                                       [ ("cb-access-key", key tok)
                                       , ("cb-access-sign", Base64.encode sign)
                                       , ("cb-access-timestamp", time)
                                       , ("cb-access-passphrase", passphrase tok)
                                       ]
                                }
            Nothing  -> throwError $ AuthenticationRequiredFailure $ T.decodeUtf8 $ path req
    where pullBody (RequestBodyBS  b) = return b
          pullBody (RequestBodyLBS b) = return $ LBS.toStrict b
          pullBody _                  = throwError AuthenticationRequiresByteStrings
signMessage False _ req = return req

--

processResponse :: (FromJSON b, MonadResource m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => Response (ResumableSource m BS.ByteString) -> m b
processResponse res =
    case responseStatus res of
        s | s == status200 -> do body <- responseBody res $$+- sinkParser (fmap fromJSON json)
                                 case body of
                                     Success b -> return b
                                     Error  er -> throwError $ ParseFailure $ T.pack er
          | otherwise      -> do body <- responseBody res $$+- CB.sinkLbs
                                 throwError $ ApiFailure $ T.decodeUtf8 $ LBS.toStrict body
