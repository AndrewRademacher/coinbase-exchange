{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.Private where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Hashable
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.UUID
import           Data.Word
import           GHC.Generics

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

-- Accounts

newtype AccountId = AccountId { unAccountId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

data Account
    = Account
        { accId        :: AccountId
        , accBalance   :: CoinScientific
        , accHold      :: CoinScientific
        , accAvailable :: CoinScientific
        , accCurrency  :: CurrencyId
        }
    deriving (Show, Eq, Data, Typeable, Generic)

instance NFData Account
instance ToJSON Account where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Account where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype EntryId = EntryId { unEntryId :: Word64 }
    deriving (Eq, Ord, Num, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

data Entry
    = Entry
        { entryId        :: EntryId
        , entryCreatedAt :: UTCTime
        , entryAmount    :: CoinScientific
        , entryBalance   :: CoinScientific
        , entryType      :: EntryType
        , entryDetails   :: EntryDetails
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Entry
instance ToJSON Entry where
    toJSON Entry{..} = object [ "id"         .= entryId
                              , "created_at" .= entryCreatedAt
                              , "amount"     .= entryAmount
                              , "balance"    .= entryBalance
                              , "type"       .= entryType
                              , "details"    .= entryDetails
                              ]
instance FromJSON Entry where
    parseJSON (Object m) = Entry
        <$> m .: "id"
        <*> m .: "created_at"
        <*> m .: "amount"
        <*> m .: "balance"
        <*> m .: "type"
        <*> m .: "details"
    parseJSON _ = mzero

data EntryType
    = Match
    | Fee
    | Transfer
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData EntryType
instance Hashable EntryType
instance ToJSON EntryType where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }
instance FromJSON EntryType where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

data EntryDetails
    = EntryDetails
        { detailOrderId   :: Maybe OrderId
        , detailTradeId   :: Maybe TradeId
        , detailProductId :: Maybe ProductId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData EntryDetails
instance ToJSON EntryDetails where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON EntryDetails where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype HoldId = HoldId { unHoldId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

data Hold
    = OrderHold
        { holdId        :: HoldId
        , holdAccountId :: AccountId
        , holdCreatedAt :: UTCTime
        , holdUpdatedAt :: UTCTime
        , holdAmount    :: CoinScientific
        , holdOrderRef  :: OrderId
        }
    | TransferHold
        { holdId          :: HoldId
        , holdAccountId   :: AccountId
        , holdCreatedAt   :: UTCTime
        , holdUpdatedAt   :: UTCTime
        , holdAmount      :: CoinScientific
        , holdTransferRef :: TransferId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Hold
instance ToJSON Hold where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Hold where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Orders
data OrderContigency
    = GoodTillCanceled
    | ImmediateOrCancel
    | FillOrKill
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData   OrderContigency
instance Hashable OrderContigency

instance ToJSON OrderContigency where
    toJSON GoodTillCanceled  = String "GTC"
    toJSON ImmediateOrCancel = String "IOC"
    toJSON FillOrKill        = String "FOK"
instance FromJSON OrderContigency where
    parseJSON (String "GTC") = return GoodTillCanceled
    parseJSON (String "IOC") = return ImmediateOrCancel
    parseJSON (String "FOK") = return FillOrKill
    parseJSON _ = mzero

data SelfTrade
    = DecrementAndCancel
    | CancelOldest
    | CancelNewest
    | CancelBoth
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData SelfTrade
instance Hashable SelfTrade
instance ToJSON SelfTrade where
    toJSON DecrementAndCancel = String "dc"
    toJSON CancelOldest       = String "co"
    toJSON CancelNewest       = String "cn"
    toJSON CancelBoth         = String "cb"
instance FromJSON SelfTrade where
    parseJSON (String "dc") = return DecrementAndCancel
    parseJSON (String "co") = return CancelOldest
    parseJSON (String "cn") = return CancelNewest
    parseJSON (String "cb") = return CancelBoth
    parseJSON _ = mzero

data NewOrder
    = NewLimitOrder
        { noProductId :: ProductId
        , noSide      :: Side
        , noSelfTrade :: SelfTrade
        , noClientOid :: Maybe ClientOrderId
        ---
        , noPrice     :: Price
        , noSize      :: Size
        ,noTimeInForce:: OrderContigency
        , noPostOnly  :: Bool
        }
    | NewMarketOrder
        { noProductId :: ProductId
        , noSide      :: Side
        , noSelfTrade :: SelfTrade
        , noClientOid :: Maybe ClientOrderId
        ---
        , noSizeAndOrFunds  :: Either Size (Maybe Size, Cost)
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData NewOrder
instance ToJSON NewOrder where
        toJSON NewLimitOrder{..} = object
           ([ "type" .= ("limit" :: Text)
            , "product_id"    .= noProductId
            , "side"          .= noSide
            , "stp"           .= noSelfTrade
            , "price"         .= noPrice
            , "size"          .= noSize
            , "time_in_force" .= noTimeInForce
            , "post_only"     .= noPostOnly
            ] ++ clientID )
            where
                clientID = case noClientOid of
                                Just cid -> [ "client_oid" .= cid ]
                                Nothing  -> []

        toJSON NewMarketOrder{..} = object
           ([ "type" .= ("market" :: Text)
            , "product_id"    .= noProductId
            , "side"          .= noSide
            , "stp"           .= noSelfTrade
            ] ++ clientID ++ size ++ funds )
            where
                clientID = case noClientOid of
                                Just cid -> [ "client_oid" .= cid ]
                                Nothing  -> []
                (size,funds) = case noSizeAndOrFunds of
                                Left  s -> (["size" .= s],[])
                                Right (ms,f) -> case ms of
                                            Nothing -> ( []            , ["funds" .= f] )
                                            Just s' -> ( ["size" .= s'], ["funds" .= f] )


data OrderConfirmation
    = OrderConfirmation
        { ocId :: OrderId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData OrderConfirmation
instance ToJSON OrderConfirmation where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON OrderConfirmation where
    parseJSON = genericParseJSON coinbaseAesonOptions

data Order
    = LimitOrder
        { orderId         :: OrderId
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderSelfTrade  :: SelfTrade
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderFilledSize :: Maybe Size
        , orderFilledFees :: Maybe Price
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason

        , orderPrice      :: Price
        , orderSize       :: Size
        , orderTimeInForce:: OrderContigency
        , orderPostOnly   :: Bool
        }
    | MarketOrder
        { orderId         :: OrderId
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderSelfTrade  :: SelfTrade
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderFilledSize :: Maybe Size
        , orderFilledFees :: Maybe Price
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason

        , orderSizeAndOrFunds  :: Either Size (Maybe Size, Cost)
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Order
instance ToJSON Order where
    toJSON LimitOrder{..} = object
        [ "type" .= ("limit" :: Text)
        , "id"            .= orderId
        , "product_id"    .= orderProductId
        , "status"        .= orderStatus
        , "stp"           .= orderSelfTrade
        , "settled"       .= orderSettled
        , "side"          .= orderSide
        , "created_at"    .= orderCreatedAt
        , "filled_size"   .= orderFilledSize
        , "filled_fees"   .= orderFilledFees
        , "done_at"       .= orderDoneAt
        , "done_reason"   .= orderDoneReason

        , "price"         .= orderPrice
        , "size"          .= orderSize
        , "time_in_force" .= orderTimeInForce
        , "post_only"     .= orderPostOnly
        ]
    toJSON MarketOrder{..} = object
       ([ "type" .= ("market" :: Text)
        , "id"            .= orderId
        , "product_id"    .= orderProductId
        , "status"        .= orderStatus
        , "stp"           .= orderSelfTrade
        , "settled"       .= orderSettled
        , "side"          .= orderSide
        , "created_at"    .= orderCreatedAt
        , "filled_size"   .= orderFilledSize
        , "filled_fees"   .= orderFilledFees
        , "done_at"       .= orderDoneAt
        , "done_reason"   .= orderDoneReason
        ] ++ size ++ funds )
            where (size,funds) = case orderSizeAndOrFunds of
                        Left  s -> (["size" .= s],[])
                        Right (ms,f) -> case ms of
                                    Nothing -> ( []            , ["funds" .= f] )
                                    Just s' -> ( ["size" .= s'], ["funds" .= f] )


instance FromJSON Order where
    parseJSON (Object m) = do
        ordertype <- m .: "type"
        case (ordertype :: String) of
            "limit" -> LimitOrder
                <$> m .: "id"
                <*> m .: "product_id"
                <*> m .: "status"
                <*> m .: "stp"
                <*> m .: "settled"
                <*> m .: "side"
                <*> m .: "created_at"
                <*> m .:? "filled_size"
                <*> m .:? "filled_fees"
                <*> m .:? "done_at"
                <*> m .:? "done_reason"
                <*> m .: "price"
                <*> m .: "size"
                <*> m .:? "time_in_force" .!= GoodTillCanceled -- older orders don't seem to have this field
                <*> m .: "post_only"

            "market" -> MarketOrder
                <$> m .: "id"
                <*> m .: "product_id"
                <*> m .: "status"
                <*> m .: "stp"
                <*> m .: "settled"
                <*> m .: "side"
                <*> m .: "created_at"
                <*> m .:? "filled_size"
                <*> m .:? "filled_fees"
                <*> m .:? "done_at"
                <*> m .:? "done_reason"
                <*> (do
                        ms <- m .:? "size"
                        mf <- m .:? "funds"
                        case (ms,mf) of
                            (Nothing, Nothing) -> mzero
                            (Just s , Nothing) -> return $ Left  s
                            (Nothing, Just f ) -> return $ Right (Nothing, f)
                            (Just s , Just f ) -> return $ Right (Just s , f)
                            )
            _ -> mzero

    parseJSON _ = mzero

-- Fills

data Liquidity
    = Maker
    | Taker
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData Liquidity
instance Hashable Liquidity
instance ToJSON Liquidity where
    toJSON Maker = String "M"
    toJSON Taker = String "T"
instance FromJSON Liquidity where
    parseJSON (String "M") = return Maker
    parseJSON (String "T") = return Taker
    parseJSON _            = mzero

data Fill
    = Fill
        { fillTradeId   :: TradeId
        , fillProductId :: ProductId
        , fillPrice     :: Price
        , fillSize      :: Size
        , fillOrderId   :: OrderId
        , fillCreatedAt :: UTCTime
        , fillLiquidity :: Liquidity
        , fillFee       :: Price
        , fillSettled   :: Bool
        , fillSide      :: Side
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Fill
instance ToJSON Fill where
    toJSON Fill{..} = object
        [ "trade_id"    .= fillTradeId
        , "product_id"  .= fillProductId
        , "price"       .= fillPrice
        , "size"        .= fillSize
        , "order_id"    .= fillOrderId
        , "created_at"  .= fillCreatedAt
        , "liquidity"   .= fillLiquidity
        , "fee"         .= fillFee
        , "settled"     .= fillSettled
        , "side"        .= fillSide
        ]
instance FromJSON Fill where
    parseJSON (Object m) = Fill
        <$> m .: "trade_id"
        <*> m .: "product_id"
        <*> m .: "price"
        <*> m .: "size"
        <*> m .: "order_id"
        <*> m .: "created_at"
        <*> m .: "liquidity"
        <*> m .: "fee"
        <*> m .: "settled"
        <*> m .: "side"
    parseJSON _ = mzero

-- Transfers

newtype TransferId = TransferId { unTransferId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, FromJSON, ToJSON)

newtype CoinbaseAccountId = CoinbaseAccountId { unCoinbaseAccountId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, FromJSON, ToJSON)

data Transfer
    = Deposit
        { transAmount          :: Size
        , transCoinbaseAccount :: CoinbaseAccountId
        }
    | Withdraw
        { transAmount          :: Size
        , transCoinbaseAccount :: CoinbaseAccountId
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Transfer
instance ToJSON Transfer where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Transfer where
    parseJSON = genericParseJSON coinbaseAesonOptions
