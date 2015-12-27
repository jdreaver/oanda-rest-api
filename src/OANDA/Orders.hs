{-# LANGUAGE DeriveGeneric #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/orders/ Orders> section of the API.


module OANDA.Orders
       ( openOrders
       , Order (..)
       ) where


import           Data.Aeson
import           Data.Decimal
import           Data.Time (ZonedTime)
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Types
import           OANDA.Util


-- | Get all open orders for an account.
openOrders :: OandaData -> AccountID -> IO (V.Vector Order)
openOrders od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid ++ "/orders"
         opts = constructOpts od []
     jsonResponseArray url opts "orders"


data Order = Order
  { orderId           :: Integer
  , orderInstrument   :: InstrumentText
  , orderUnits        :: Integer
  , orderSide         :: Side
  , orderType         :: String -- "marketIfTouched",
  , orderTime         :: ZonedTime
  , orderPrice        :: Decimal
  , orderTakeProfit   :: Decimal
  , orderStopLoss     :: Decimal
  , orderExpiry       :: ZonedTime
  , orderUpperBound   :: Decimal
  , orderLowerBound   :: Decimal
  , orderTrailingStop :: Decimal
  } deriving (Show, Generic)


instance FromJSON Order where
  parseJSON = genericParseJSON $ jsonOpts "order"
