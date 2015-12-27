{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/positions/ Positions> section of the
-- API.

module OANDA.Positions
       ( Position (..)
       , openPositions
       , position
       , closePosition
       , CloseResponse (..)
       ) where

import           Data.Aeson
import           Data.Decimal
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types

-- | Get all open positions for an account.
openPositions :: OandaEnv -> AccountID -> IO (V.Vector Position)
openPositions od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions"
         opts = constructOpts od []
     jsonResponseArray url opts "positions"

data Position = Position
  { positionInstrument :: String
  , positionUnits      :: Int
  , positionSide       :: Side
  , positionAvgPrice   :: Decimal
  } deriving (Show, Generic)

instance FromJSON Position where
  parseJSON = genericParseJSON $ jsonOpts "position"


-- | Get open position for an account on a given instrument.
position :: OandaEnv -> AccountID -> InstrumentText -> IO Position
position od (AccountID aid) ins =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions/" ++ ins
         opts = constructOpts od []
     jsonResponse url opts


-- | Closes an existing position.
closePosition :: OandaEnv -> AccountID -> InstrumentText -> IO CloseResponse
closePosition od (AccountID aid) ins =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions/" ++ ins
         opts = constructOpts od []
     jsonDelete url opts


data CloseResponse = CloseResponse
  { closeResponseIds        :: V.Vector Int
  , closeResponseInstrument :: InstrumentText
  , closeResponseTotalUnits :: Int
  , closeResponsePrice      :: Decimal
  } deriving (Show, Generic)


instance FromJSON CloseResponse where
  parseJSON = genericParseJSON $ jsonOpts "closeResponse"
