-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/pricing-ep/ Pricing> section of
-- the API.

module OANDA.Pricing where

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
  , priceTime :: ZonedTime
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
