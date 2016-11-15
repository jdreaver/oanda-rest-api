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

import qualified Data.Vector as V

import OANDA.Internal

-- | Get all open positions for an account.
openPositions :: OandaEnv -> AccountID -> IO (V.Vector Position)
openPositions od (AccountID aid) = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions"
  request <- constructRequest od url []
  jsonResponseArray request "positions"

data Position = Position
  { positionInstrument :: Text
  , positionUnits      :: Int
  , positionSide       :: Side
  , positionAvgPrice   :: Decimal
  } deriving (Show, Generic)

instance FromJSON Position where
  parseJSON = genericParseJSON $ jsonOpts "position"

-- | Get open position for an account on a given instrument.
position :: OandaEnv -> AccountID -> InstrumentText -> IO Position
position od (AccountID aid) ins = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions/" ++ unpack ins
  request <- constructRequest od url []
  jsonResponse request

-- | Closes an existing position.
closePosition :: OandaEnv -> AccountID -> InstrumentText -> IO CloseResponse
closePosition od (AccountID aid) ins = do
  let url = "DELETE " ++ baseURL od ++ "/v1/accounts/" ++ show aid ++ "/positions/" ++ unpack ins
  request <- constructRequest od url []
  jsonResponse request

data CloseResponse = CloseResponse
  { closeResponseIds        :: V.Vector Int
  , closeResponseInstrument :: InstrumentText
  , closeResponseTotalUnits :: Int
  , closeResponsePrice      :: Decimal
  } deriving (Show, Generic)

instance FromJSON CloseResponse where
  parseJSON = genericParseJSON $ jsonOpts "closeResponse"
