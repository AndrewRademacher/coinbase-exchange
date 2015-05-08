{-# LANGUAGE CPP                        #-}
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
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format    (dateTimeFmt, defaultTimeLocale)
#else
import           System.Locale       (dateTimeFmt, defaultTimeLocale)
#endif


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
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance ToJSON TradeId where
    toJSON = String . T.pack . show . unTradeId

instance FromJSON TradeId where
    parseJSON (String t) = pure $ TradeId $ read $ T.unpack t
    parseJSON (Number n) = pure $ TradeId $ floor n
    parseJSON _ = mzero

--

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
    deriving (Eq, Ord, Show, Read, IsString, FromJSON, ToJSON)

----

data OrderStatus
    = Done
    | Settled
    | Open
    | Pending
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON OrderStatus where
    toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON OrderStatus where
    parseJSON = genericParseJSON coinbaseAesonOptions

--

newtype ClientOrderId = ClientOrderId { unClientOrderId :: UUID }
    deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

--

data Reason = Filled | Canceled
    deriving (Eq, Show, Read, Generic)

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
        case maybeRead (T.unpack t) of
            Just  n -> pure $ CoinScientific n
            Nothing -> fail "Could not parse string scientific."

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

----

newtype CoinbaseTime = CoinbaseTime { unCoinbaseTime :: UTCTime }
    deriving (Eq, Ord, Show, Read, Data, Typeable, NFData)

instance ToJSON CoinbaseTime where
    toJSON (CoinbaseTime t) = String $ T.pack $
        formatTime defaultTimeLocale coinbaseTimeFormat t ++ "00"

instance FromJSON CoinbaseTime where
    parseJSON = withText "Coinbase Time" $ \t ->
        case parseTime defaultTimeLocale coinbaseTimeFormat (T.unpack t ++ "00") of
            Just d -> pure $ CoinbaseTime d
            _      -> fail "could not parse coinbase time format."

coinbaseTimeFormat :: String
coinbaseTimeFormat = "%F %T%Q%z"

----

coinbaseAesonOptions :: Options
coinbaseAesonOptions = (aesonPrefix snakeCase)
    { constructorTagModifier = map toLower
    , sumEncoding = defaultTaggedObject
                        { tagFieldName = "type"
                        }
    }
