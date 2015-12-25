{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/rates/ Rates> section of the API.

module OANDA.Rates
       ( InstrumentsArgs (..)
       , instrumentsArgs
       , Instrument (..)
       , instruments
       ) where

import           Data.Aeson
import           Data.Text (Text, pack)
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types


data InstrumentsArgs = InstrumentsArgs
  { instrumentsFields      :: [Text]
  , instrumentsInstruments :: [Text]
  } deriving (Show)

instrumentsArgs :: InstrumentsArgs
instrumentsArgs = InstrumentsArgs ["displayName", "pip", "maxTradeUnits"] []

data Instrument = Instrument
  { instrumentInstrument    :: String
  , instrumentPip           :: Maybe String
  , instrumentMaxTradeUnits :: Maybe Double
  , instrumentDisplayName   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Instrument where
  parseJSON = genericParseJSON $ jsonOpts "instrument"

-- | Retrieve a list of instruments from OANDA
instruments :: OandaData -> AccountID -> InstrumentsArgs -> IO (V.Vector Instrument)
instruments od (AccountID aid) (InstrumentsArgs fs is) = do
  let url = baseURL od ++ "/v1/instruments"
      opts = constructOpts od [ ("accountId", [pack $ show aid])
                              , ("instruments", is)
                              , ("fields", fs)
                              ]
  jsonResponseArray url opts "instruments"
