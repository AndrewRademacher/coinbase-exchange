{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbase.Exchange.Types.Private where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Data
import           Data.String
import           Data.Text                    (Text)
import           GHC.Generics

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

newtype AccountId = AccountId { unAccountId :: Text }
    deriving (Eq, Show, Read, IsString, Data, Typeable, Generic, FromJSON, ToJSON)

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
