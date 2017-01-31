{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Coinbase.Exchange.Types.Socket where

import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Types      hiding (Error)
import           Data.Data
import           Data.Hashable
import           Data.Text                    (Text)
import           Data.Time
import           Data.Word
import           GHC.Generics
import qualified Data.HashMap.Strict as H

import           Coinbase.Exchange.Types.Core  hiding (OrderStatus(..))

data ExchangeMessage
    = Subscribe
        { msgProductId :: ProductId
        }
    | HeartbeatReq
        { msgHeartbeatOn :: Bool }
    | Heartbeat
        { msgTime        :: UTCTime
        , msgProductId   :: ProductId
        , msgSequence    :: Sequence
        , msgLastTradeId :: TradeId
        }
    | ReceivedLimit
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgClientOid :: Maybe ClientOrderId
        --
        , msgPrice     :: Price
        , msgSize      :: Size
        }
    | ReceivedMarket
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgClientOid :: Maybe ClientOrderId
        -- market orders have no price and are bounded by either size, funds or both
        , msgMarketBounds :: (Either Size (Maybe Size, Cost))
        }
    | Open
        { msgTime          :: UTCTime
        , msgProductId     :: ProductId
        , msgSequence      :: Sequence
        , msgOrderId       :: OrderId
        , msgSide          :: Side
        , msgRemainingSize :: Size
        , msgPrice         :: Price
        }
    | Match
        { msgTime         :: UTCTime
        , msgProductId    :: ProductId
        , msgSequence     :: Sequence
        , msgSide         :: Side
        , msgTradeId      :: TradeId
        , msgMakerOrderId :: OrderId
        , msgTakerOrderId :: OrderId
        , msgSize         :: Size
        , msgPrice        :: Price
        }
    | Done
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgReason    :: Reason
        -- It is possible for these next two fields to be Nothing separately
        -- Filled market orders limited by funds will not have a price but may have remaining_size
        -- Filled limit orders may have a price but not a remaining_size (assumed zero)
        -- CURRENTLY ** `remaining_size` reported in Done messages is sometimes incorrect **
        -- This appears to be bug at GDAX. I've told them about it.
        , msgMaybePrice   :: Maybe Price
        , msgMaybeRemSize :: Maybe Size
        }
    | ChangeLimit
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgPrice     :: Price
        , msgNewSize   :: Size
        , msgOldSize   :: Size
        }
    | ChangeMarket
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgNewFunds  :: Cost
        , msgOldFunds  :: Cost
        }
    | Error
        { msgMessage :: Text
        }
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData ExchangeMessage

-----------------------------
instance FromJSON ExchangeMessage where
    parseJSON (Object m) = do
        msgtype <- m .: "type"
        -- TO DO: `HeartbeatReq` and `Subscribe` message types are missing as those are
        -- never received by the client.
        case (msgtype :: String) of
            "hearbeat"-> Heartbeat
                <$> m .: "time"
                <*> m .: "product_id"
                <*> m .: "sequence"
                <*> m .: "last_trade_id"
            "open" -> Open
                <$> m .: "time"
                <*> m .: "product_id"
                <*> m .: "sequence"
                <*> m .: "order_id"
                <*> m .: "side"
                <*> m .: "remaining_size"
                <*> m .: "price"
            "done" -> Done
                <$> m .: "time"
                <*> m .: "product_id"
                <*> m .: "sequence"
                <*> m .: "order_id"
                <*> m .: "side"
                <*> m .: "reason"
                <*> m .:? "price"
                <*> m .:? "remaining_size"
            "match" -> Match
                <$> m .: "time"
                <*> m .: "product_id"
                <*> m .: "sequence"
                <*> m .: "side"
                <*> m .: "trade_id"
                <*> m .: "maker_order_id"
                <*> m .: "taker_order_id"
                <*> m .: "size"
                <*> m .: "price"
            "change" -> do
                ms <- m .:? "new_size"
                case (ms :: Maybe Size) of
                    Nothing -> ChangeMarket
                                <$> m .: "time"
                                <*> m .: "product_id"
                                <*> m .: "sequence"
                                <*> m .: "order_id"
                                <*> m .: "side"
                                <*> m .: "new_funds"
                                <*> m .: "old_funds"
                    Just _ -> ChangeLimit
                                <$> m .: "time"
                                <*> m .: "product_id"
                                <*> m .: "sequence"
                                <*> m .: "order_id"
                                <*> m .: "side"
                                <*> m .: "price"
                                <*> m .: "new_size"
                                <*> m .: "old_size"
            "received" -> do
                typ  <- m .:  "order_type"
                mcid <- m .:? "client_id"
                case typ of
                    Limit -> ReceivedLimit
                                <$> m .: "time"
                                <*> m .: "product_id"
                                <*> m .: "sequence"
                                <*> m .: "order_id"
                                <*> m .: "side"
                                <*> pure (mcid :: Maybe ClientOrderId)
                                <*> m .: "price"
                                <*> m .: "size"
                    Market -> ReceivedMarket
                                <$> m .: "time"
                                <*> m .: "product_id"
                                <*> m .: "sequence"
                                <*> m .: "order_id"
                                <*> m .: "side"
                                <*> pure mcid
                                <*> (do
                                        -- I can't try to parse "size" or "funds" with (.:?) here, their type is CoinScientific
                                        -- but the fields may be "size":null and that will fail the (m .:? "size") parser.
                                        ms <- m .:?? "size"
                                        mf <- m .:?? "funds"
                                        case (ms,mf) of
                                            (Nothing, Nothing) -> mzero
                                            (Just s , Nothing) -> return $ Left  s
                                            (Nothing, Just f ) -> return $ Right (Nothing, f)
                                            (Just s , Just f ) -> return $ Right (Just s , f)
                                            )
            "error" -> error (show m)

    parseJSON _ = mzero

---------------------------
-- This is based on the code for Aeson's (.:?) operator. Except, we're more
-- lax than (.:?) and also return 'Nothing' when the field is (JSON) null.
(.:??) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> if v == Null
                   then pure Nothing
                   else obj .:? key

---------------------------

instance ToJSON ExchangeMessage where
    toJSON Subscribe{..} = object
        [ "type"       .= ("subscribe" :: Text)
        , "product_id" .= msgProductId
        ]
    -- TO DO: `Heartbeat` message type is missing as those messages
    -- are never sent by the client.
    toJSON HeartbeatReq{..} = object
        [ "type"       .= ("heartbeat" :: Text)
        , "on"         .= msgHeartbeatOn]
    toJSON Open{..} = object
        [ "type"       .= ("open" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "remaining_size" .= msgRemainingSize
        , "price"      .= msgPrice
        ]
    toJSON Done{..} = object
        ([ "type"      .= ("done" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "reason"     .= msgReason
        ]
        ++ case msgMaybePrice of
                Nothing -> []
                Just  p -> ["price" .= p]
        ++ case msgMaybeRemSize of
                Nothing -> []
                Just  s -> ["remaining_size" .= s]
        )
    toJSON Match{..} = object
        [ "type"       .= ("match" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "side"       .= msgSide
        , "trade_id"   .= msgTradeId
        , "maker_order_id" .= msgMakerOrderId
        , "taker_order_id" .= msgTakerOrderId
        , "size"       .= msgSize
        , "price"      .= msgPrice
        ]
    toJSON Error{..} = object
        [ "type" .= ("error" :: Text)
        , "message" .= msgMessage
        ]
    toJSON ChangeLimit{..} = object
        [ "type"       .= ("change" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "new_size"   .= msgNewSize
        , "old_size"   .= msgOldSize
        , "price"      .= msgPrice
        ]
    toJSON ChangeMarket{..} = object
        [ "type"       .= ("change" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "new_funds"  .= msgNewFunds
        , "old_funds"  .= msgOldFunds
        ]

    toJSON ReceivedLimit{..} = object (
        [ "type"       .= ("received" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "size"       .= msgSize
        , "price"      .= msgPrice
        , "order_type" .= Limit
        ] ++ clientID)
            where
                clientID = case msgClientOid of
                    Nothing -> []
                    Just ci -> ["client_id" .= msgClientOid ]

    toJSON ReceivedMarket{..} = object (
        ["type"       .= ("received" :: Text)
        , "time"       .= msgTime
        , "product_id" .= msgProductId
        , "sequence"   .= msgSequence
        , "order_id"   .= msgOrderId
        , "side"       .= msgSide
        , "order_type" .= Market
        ] ++ clientID ++ size ++ funds)
            where
                clientID = case msgClientOid of
                    Nothing -> []
                    Just ci -> ["client_id" .= msgClientOid ]
                (size,funds) = case msgMarketBounds of
                    Left  s -> (["size" .= s],[])
                    Right (ms,f) -> case ms of
                                Nothing -> ( []            , ["funds" .= f] )
                                Just s' -> ( ["size" .= s'], ["funds" .= f] )
