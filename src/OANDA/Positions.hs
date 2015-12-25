{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/positions/ Positions> section of the
-- API.

module OANDA.Positions
       ( Position (..)
       , openPositions
       , position
       ) where

import           Data.Aeson
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types

-- | Get all open positions for an account.
openPositions :: OandaData -> AccountID -> IO (V.Vector Position)
openPositions od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions"
         opts = constructOpts od []
     jsonResponseArray url opts "positions"

data Position = Position
  { positionInstrument :: String
  , positionUnits      :: Int
  , positionSide       :: Side
  , positionAvgPrice   :: Double
  } deriving (Show, Generic)

instance FromJSON Position where
  parseJSON = genericParseJSON $ jsonOpts "position"


-- | Get open position for an account on a given instrument.
position :: OandaData -> AccountID -> InstrumentText -> IO Position
position od (AccountID aid) ins =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions/" ++ ins
         opts = constructOpts od []
     jsonResponse url opts
