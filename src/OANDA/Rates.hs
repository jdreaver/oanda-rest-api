{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/rates/ Rates> section of the API.

module OANDA.Rates
  ( InstrumentsArgs (..)
  , instrumentsArgs
  , Instrument (..)
  , instruments
  , Price (..)
  , prices
  ) where

import qualified Data.Vector as V

import OANDA.Internal


data InstrumentsArgs = InstrumentsArgs
  { instrumentsFields      :: Maybe [Text]
  , instrumentsInstruments :: Maybe [Text]
  } deriving (Show)

instrumentsArgs :: InstrumentsArgs
instrumentsArgs = InstrumentsArgs Nothing Nothing

data Instrument = Instrument
  { instrumentInstrument      :: Text
  , instrumentPip             :: Maybe Decimal
  , instrumentMaxTradeUnits   :: Maybe Integer
  , instrumentDisplayName     :: Maybe Text
  , instrumentPrecision       :: Maybe Decimal
  , instrumentMaxTrailingStop :: Maybe Decimal
  , instrumentMinTrailingStop :: Maybe Decimal
  , instrumentMarginRate      :: Maybe Decimal
  , instrumentHalted          :: Maybe Bool
  , instrumentInterestRate    :: Maybe Object
  } deriving (Show, Generic)

instance FromJSON Instrument where
  parseJSON = genericParseJSON $ jsonOpts "instrument"

-- | Retrieve a list of instruments from OANDA
instruments :: OandaEnv -> AccountID -> InstrumentsArgs -> IO (V.Vector Instrument)
instruments od (AccountID aid) (InstrumentsArgs fs is) = do
  let url = "GET " ++ baseURL od ++ "/v1/instruments"
  request <- constructRequest od url
    [ ("accountId", Just [pack $ show aid])
    , ("instruments", is)
    , ("fields", fs)
    ]
  jsonResponseArray request "instruments"

-- | Retrieve the current prices for a list of instruments.
prices :: OandaEnv -> [InstrumentText] -> Maybe ZonedTime -> IO (V.Vector Price)
prices od is zt = do
  let
    url = "GET " ++ baseURL od ++ "/v1/prices"
    ztOpt = maybe [] (\zt' -> [("since", Just [pack $ formatTimeRFC3339 zt'])]) zt
  request <- constructRequest od url (("instruments", Just is) : ztOpt)
  jsonResponseArray request "prices"

data Price = Price
  { priceInstrument :: InstrumentText
  , priceTime       :: ZonedTime
  , priceBid        :: Decimal
  , priceAsk        :: Decimal
  } deriving (Show, Generic)

instance FromJSON Price where
  parseJSON = genericParseJSON $ jsonOpts "price"
