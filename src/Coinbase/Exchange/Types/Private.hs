{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Coinbase.Exchange.Types.Private where

import           Control.Monad
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Text                    (Text)
import           Data.Time
import           Data.UUID
import           GHC.Generics

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

-- Accounts

newtype AccountId = AccountId { unAccountId :: UUID }
    deriving (Eq, Show, Read, Data, Typeable, Generic, FromJSON, ToJSON)

data Account
    = Account
        { accId        :: AccountId
        , accBalance   :: CoinScientific
        , accHold      :: CoinScientific
        , accAvailable :: CoinScientific
        , accCurrency  :: CurrencyId
        }
    deriving (Show, Generic)

instance ToJSON Account where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Account where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype EntryId = EntryId { unEntryId :: UUID }
    deriving (Eq, Show, Read, Data, Typeable, Generic, FromJSON, ToJSON)

data Entry
    = Entry
        { entryId        :: EntryId
        , entryCreatedAt :: UTCTime
        , entryAmount    :: CoinScientific
        , entryBalance   :: CoinScientific
        , entryType      :: EntryType
        , entryDetails   :: EntryDetails
        }
    deriving (Show, Generic)

instance ToJSON Entry where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Entry where
    parseJSON = genericParseJSON coinbaseAesonOptions

data EntryType
    = Match
    | Fee
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON EntryType where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }

instance FromJSON EntryType where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

data EntryDetails
    = EntryDetails
        { detailOrderId   :: OrderId
        , detailTradeId   :: TradeId
        , detailProductId :: ProductId
        }
    deriving (Show, Generic)

instance ToJSON EntryDetails where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON EntryDetails where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype HoldId = HoldId { unHoldId :: UUID }
    deriving (Eq, Show, Read, Data, Typeable, Generic, FromJSON, ToJSON)

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
    deriving (Show, Generic)

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
    deriving (Eq, Show, Read, Data, Typeable, Generic)

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
        , noSelfTrade :: SelfTrade
        }
    deriving (Show, Generic)

instance ToJSON NewOrder where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON NewOrder where
    parseJSON = genericParseJSON coinbaseAesonOptions

data OrderConfirmation
    = OrderConfirmation
        { ocId :: OrderId
        }
    deriving (Show, Generic)

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
        , orderFilledSize :: Size
        , orderFilledFees :: Price
        , orderSettled    :: Bool
        , orderSide       :: Side
        , orderCreatedAt  :: UTCTime
        , orderDoneAt     :: Maybe UTCTime
        , orderDoneReason :: Maybe Reason
        }
    deriving (Show, Generic)

instance ToJSON Order where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Order where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Fills

data Liquidity
    = Maker
    | Taker
    deriving (Eq, Show, Read, Data, Typeable, Generic)

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
    deriving (Show, Generic)

instance ToJSON Fill where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Fill where
    parseJSON = genericParseJSON coinbaseAesonOptions

-- Transfers

newtype TransferId = TransferId { unTransferId :: UUID }
    deriving (Eq, Show, Read, Data, Typeable, Generic, FromJSON, ToJSON)
