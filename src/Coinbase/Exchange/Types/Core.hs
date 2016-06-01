{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbase.Exchange.Types.Core where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Data
import           Data.Hashable
import           Data.Int
import           Data.Maybe
import           Data.Scientific
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           Data.UUID
import           Data.UUID.Aeson     ()
import           Data.Word
import           GHC.Generics
import           Text.Read           (readMaybe)


newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, IsString, NFData, Hashable, FromJSON, ToJSON)

newtype Price = Price { unPrice :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Size = Size { unSize :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Cost = Cost { unCost :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype OrderId = OrderId { unOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Aggregate = Aggregate { unAggregate :: Int64 }
    deriving (Eq, Ord, Show, Read, Num, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

newtype Sequence = Sequence { unSequence :: Word64 }
    deriving (Eq, Ord, Num, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

--

data Side = Buy | Sell
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData Side
instance Hashable Side
instance ToJSON Side where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON Side where
    parseJSON = genericParseJSON coinbaseAesonOptions

--
data OrderType = Limit | Market
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance NFData OrderType
instance Hashable OrderType
instance ToJSON OrderType where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON OrderType where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype TradeId = TradeId { unTradeId :: Word64 }
    deriving (Eq, Ord, Num, Show, Read, Data, Typeable, Generic, NFData, Hashable)

instance ToJSON TradeId where
    toJSON = String . T.pack . show . unTradeId
instance FromJSON TradeId where
    parseJSON (String t) = pure $ TradeId $ read $ T.unpack t
    parseJSON (Number n) = pure $ TradeId $ floor n
    parseJSON _ = mzero

--

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, IsString, NFData, Hashable, FromJSON, ToJSON)

----

data OrderStatus
    = Done
    | Settled
    | Open
    | Pending
    | Active
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData OrderStatus
instance Hashable OrderStatus
instance ToJSON OrderStatus where
    toJSON = genericToJSON coinbaseAesonOptions
instance FromJSON OrderStatus where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, NFData, Hashable, FromJSON, ToJSON)

--

data Reason = Filled | Canceled
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData Reason
instance Hashable Reason
instance ToJSON Reason where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }
instance FromJSON Reason where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower }

----

newtype CoinScientific = CoinScientific { unCoinScientific :: Scientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, NFData, Hashable)

instance ToJSON CoinScientific where
    toJSON (CoinScientific v) = String . T.pack . show $ v
instance FromJSON CoinScientific where
    parseJSON = withText "CoinScientific" $ \t ->
        case readMaybe (T.unpack t) of
            Just  n -> pure $ CoinScientific n
            Nothing -> fail "Could not parse string scientific."

----

coinbaseAesonOptions :: Options
coinbaseAesonOptions = (aesonPrefix snakeCase)
    { constructorTagModifier = map toLower
    , sumEncoding = defaultTaggedObject
                        { tagFieldName = "type"
                        }
    }
