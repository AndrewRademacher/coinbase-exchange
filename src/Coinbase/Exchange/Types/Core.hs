{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbase.Exchange.Types.Core where

import           Control.Applicative
import           Control.DeepSeq
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
import           Data.UUID
import           Data.UUID.Aeson     ()
import           Data.Word
import           GHC.Generics

newtype ProductId = ProductId { unProductId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

newtype Price = Price { unPrice :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, NFData, Hashable, FromJSON, ToJSON)

newtype Size = Size { unSize :: CoinScientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, NFData, Hashable, FromJSON, ToJSON)

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

--

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

--

newtype CoinScientific = CoinScientific { unCoinScientific :: Scientific }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Show, Read, Data, Typeable, NFData, Hashable)

instance ToJSON CoinScientific where
    toJSON (CoinScientific v) = String . T.pack . show $ v

instance FromJSON CoinScientific where
    parseJSON = withText "CoinScientific" $ \t ->
        case maybeRead (T.unpack t) of
            Just  n -> pure $ CoinScientific n
            Nothing -> fail "Could not parse string scientific."

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

----

coinbaseSumTypeOptions :: Options
coinbaseSumTypeOptions = (aesonPrefix snakeCase)
    { constructorTagModifier = map toLower
    , sumEncoding = defaultTaggedObject
                        { tagFieldName = "type"
                        }
    }
