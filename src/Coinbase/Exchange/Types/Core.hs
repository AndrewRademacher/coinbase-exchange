{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbase.Exchange.Types.Core where

import           Data.Aeson.Types
import           Data.Char
import           Data.Int
import           Data.Scientific
import           Data.String
import           Data.Text        (Text)
import           Data.UUID
import           Data.UUID.Aeson  ()
import           Data.Word
import           GHC.Generics

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

newtype Price = Price { unPrice :: Scientific }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Size = Size { unSize :: Scientific }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype OrderId = OrderId { unOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

newtype Aggregate = Aggregate { unAggregate :: Int64 }
    deriving (Eq, Ord, Show, Read, Num, FromJSON, ToJSON)

--

data Side = Buy | Sell
    deriving (Eq, Show, Read, Generic)

instance ToJSON Side where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }

instance FromJSON Side where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

--

newtype TradeId = TradeId { unTradeId :: Word64 }
    deriving (Eq, Ord, Show, Read, Num, FromJSON, ToJSON)
