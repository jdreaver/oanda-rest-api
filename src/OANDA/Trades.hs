{-# LANGUAGE DeriveGeneric #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/trades/ Trades History> section of the
-- API.

module OANDA.Trades
  ( Trade (..)
  , openTrades
  , tradeInfo
  ) where

import qualified Data.Vector as V

import OANDA.Internal

data Trade = Trade
  { tradeId             :: !Int
  , tradeUnits          :: !Int
  , tradeSide           :: !Side
  , tradeInstrument     :: !Text
  , tradeTime           :: !ZonedTime
  , tradePrice          :: !Decimal
  , tradeTakeProfit     :: !Decimal
  , tradeStopLoss       :: !Decimal
  , tradeTrailingStop   :: !Decimal
  , tradeTrailingAmount :: !Decimal
  } deriving (Show, Generic)

instance FromJSON Trade where
  parseJSON = genericParseJSON $ jsonOpts "trade"

-- | Gets a list of all open trades in an account.
openTrades :: OandaEnv -> AccountID -> IO (V.Vector Trade)
openTrades od (AccountID aid) = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts/" ++ show aid ++ "/trades"
  request <- constructRequest od url []
  jsonResponseArray request "trades"

type TradeID = Int

-- | Get info for a specific trade.
tradeInfo :: OandaEnv -> AccountID -> TradeID -> IO Trade
tradeInfo od (AccountID aid) tid = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts/" ++ show aid ++ "/trades/" ++ show tid
  request <- constructRequest od url []
  jsonResponse request
