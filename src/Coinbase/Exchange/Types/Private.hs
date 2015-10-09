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
    deriving (Show, Data, Typeable, Generic)

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
    = NewOrder
        { noSize      :: Size
        , noPrice     :: Price
        , noSide      :: Side
        , noProductId :: ProductId
        , noClientOid :: Maybe ClientOrderId
        , noSelfTrade :: Maybe SelfTrade
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData NewOrder
instance ToJSON NewOrder where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON NewOrder where
    parseJSON = genericParseJSON coinbaseAesonOptions

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
    = Order
        { orderId         :: OrderId
        , orderSize       :: Size
        , orderPrice      :: Price
        , orderProductId  :: ProductId
        , orderStatus     :: OrderStatus
        , orderFilledSize :: Maybe Size
        , orderFilledFees :: Maybe Price
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason
        }
    deriving (Show, Data, Typeable, Generic)

instance NFData Order
instance ToJSON Order where
    toJSON Order{..} = object
        [ "id"          .= orderId
        , "size"        .= orderSize
        , "price"       .= orderPrice
        , "product_id"  .= orderProductId
        , "status"      .= orderStatus
        , "filled_size" .= orderFilledSize
        , "filled_fees" .= orderFilledFees
        , "settled"     .= orderSettled
        , "side"        .= orderSide
        , "created_at"  .= orderCreatedAt
        , "done_at"     .= orderDoneAt
        , "done_reason" .= orderDoneReason
        ]
instance FromJSON Order where
    parseJSON (Object m) = Order
        <$> m .: "id"
        <*> m .: "size"
        <*> m .: "price"
        <*> m .: "product_id"
        <*> m .: "status"
        <*> m .:? "filled_size"
        <*> m .:? "filled_fees"
        <*> m .: "settled"
        <*> m .: "side"
        <*> m .: "created_at"
        <*> m .:? "done_at"
        <*> m .:? "done_reason"
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
