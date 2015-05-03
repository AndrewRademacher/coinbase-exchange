{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbase.Exchange.Types.Private where

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
    toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Account where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

--

data Entry
    = Entry
        { entryId        :: Text
        , entryCreatedAt :: UTCTime
        , entryAmount    :: CoinScientific
        , entryBalance   :: CoinScientific
        , entryType      :: EntryType
        , entryDetails   :: EntryDetails
        }
    deriving (Show, Generic)

instance ToJSON Entry where
    toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Entry where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

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
    toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON EntryDetails where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
