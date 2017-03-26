-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/pricing-ep/ Pricing> section of
-- the API.

module OANDA.Pricing where

import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)

import OANDA.Instrument
import OANDA.Internal

data PriceBucket
  = PriceBucket
  { priceBucketPrice :: PriceValue
  , priceBucketLiquidity :: Integer
  } deriving (Show)

deriveJSON (unPrefix "priceBucket") ''PriceBucket

data Price
  = Price
  { priceInstrument :: InstrumentName
  , priceTime :: OandaZonedTime
  , priceStatus :: Text
  , priceBids :: [PriceBucket]
  , priceAsks :: [PriceBucket]
  , priceCloseoutBid :: PriceValue
  , priceCloseoutAsk :: PriceValue
  } deriving (Show)

deriveJSON (unPrefix "price") ''Price

data PricingArgs
  = PricingArgs
  { _pricingArgsInstruments :: [InstrumentName]
  , _pricingArgsSince :: Maybe ZonedTime
  } deriving (Show)

makeLenses ''PricingArgs

pricingArgs :: [InstrumentName] -> PricingArgs
pricingArgs instruments =
  PricingArgs
  { _pricingArgsInstruments = instruments
  , _pricingArgsSince = Nothing
  }

data PricingResponse
  = PricingResponse
  { pricingResponsePrices :: [Price]
  } deriving (Show)

deriveJSON (unPrefix "pricingResponse") ''PricingResponse

oandaPricing :: OandaEnv -> AccountID -> PricingArgs -> OANDARequest PricingResponse
oandaPricing env (AccountID accountId) PricingArgs{..} = OANDARequest request
  where
    request =
      baseApiRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/pricing")
      & setRequestQueryString params
    params =
      catMaybes
      [ Just ("instruments", Just . fromString . intercalate "," . fmap (unpack . unInstrumentName) $ _pricingArgsInstruments)
      , ("since",) . Just . fromString . formatTimeRFC3339 <$> _pricingArgsSince
      ]

data PricingStreamArgs
  = PricingStreamArgs
  { _pricingStreamArgsInstruments :: [InstrumentName]
  , _pricingStreamArgsSnapshot :: Maybe Bool
  } deriving (Show)

makeLenses ''PricingStreamArgs

pricingStreamArgs :: [InstrumentName] -> PricingStreamArgs
pricingStreamArgs instruments =
  PricingStreamArgs
  { _pricingStreamArgsInstruments = instruments
  , _pricingStreamArgsSnapshot = Nothing
  }

data PricingHeartbeat
  = PricingHeartbeat
  { pricingHeartbeatTime :: OandaZonedTime
  } deriving (Show)

deriveJSON (unPrefix "pricingHeartbeat") ''PricingHeartbeat

data PricingStreamResponse
  = StreamPricingHeartbeat PricingHeartbeat
  | StreamPrice Price
  deriving (Show)

-- The ToJSON instance is just for debugging, it's not actually correct
deriveToJSON defaultOptions ''PricingStreamResponse

instance FromJSON PricingStreamResponse where
  parseJSON (Object o) = do
    type' <- o .: "type" :: Parser String
    case type' of
      "HEARTBEAT" -> StreamPricingHeartbeat <$> parseJSON (Object o)
      _ -> StreamPrice <$> parseJSON (Object o)
  parseJSON _ = mempty

oandaPricingStream :: OandaEnv -> AccountID -> PricingStreamArgs -> OANDAStreamingRequest PricingStreamResponse
oandaPricingStream env (AccountID accountId) PricingStreamArgs{..} = OANDAStreamingRequest request
  where
    request =
      baseStreamingRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/pricing/stream")
      & setRequestQueryString params
    params =
      catMaybes
      [ Just ("instruments", Just . fromString . intercalate "," . fmap (unpack . unInstrumentName) $ _pricingStreamArgsInstruments)
      , ("since",) . Just . BSL.toStrict . encode <$> _pricingStreamArgsSnapshot
      ]
