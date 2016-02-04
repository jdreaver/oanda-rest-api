{-# LANGUAGE DeriveGeneric #-}

module OANDA.Trades
       ( Trade (..)
       , openTrades
       , tradeInfo
       ) where

import           Data.Aeson
import           Data.Decimal
import           Data.Time (ZonedTime)
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Types
import           OANDA.Util

data Trade = Trade
  { tradeId             :: !Int
  , tradeUnits          :: !Int
  , tradeSide           :: !Side
  , tradeInstrument     :: !String
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
openTrades od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/trades"
         opts = constructOpts od []
     jsonResponseArray url opts "trades"


type TradeID = Int

-- | Get info for a specific trade.
tradeInfo :: OandaEnv -> AccountID -> TradeID -> IO Trade
tradeInfo od (AccountID aid) tid =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/trades/" ++ show tid
         opts = constructOpts od []
     jsonResponse url opts
