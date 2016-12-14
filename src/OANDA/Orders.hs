-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/orders/ Orders> section of the API.

module OANDA.Orders
  ( openOrders
  , Order (..)
  , createOrder
  , CreateOrderBody (..)
  , createOrderBody
  , OANDAOrderType (..)
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import OANDA.Internal

-- | Get all open orders for an account.
openOrders :: OandaEnv -> AccountID -> IO (V.Vector Order)
openOrders od (AccountID aid) = do
  let url = "GET " ++ apiBaseURL od ++ "/v1/accounts/" ++ show aid ++ "/orders"
  request <- constructRequest od url []
  jsonResponseArray request "orders"

data Order = Order
  { orderId           :: Integer
  , orderInstrument   :: InstrumentText
  , orderUnits        :: Integer
  , orderSide         :: Side
  , orderType         :: OANDAOrderType
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

createOrder :: OandaEnv -> AccountID -> CreateOrderBody -> IO Order
createOrder od (AccountID aid) CreateOrderBody {..} = do
  let
    url = "POST " ++ apiBaseURL od ++ "/v1/accounts/" ++ show aid ++ "/orders"
    (params :: [(BS.ByteString, BS.ByteString)]) = catMaybes
      [ Just ("instrument", encodeUtf8 createOrderBodyInstrument)
      , Just ("units", BS.pack $ show createOrderBodyUnits)
      , Just ("side", BS.pack $ show createOrderBodySide)
      , Just ("type", BS.pack $ show createOrderBodyType)
      , ("expiry",) . BS.pack . show <$> createOrderBodyExpiry
      , ("price",) . BS.pack . show <$> createOrderBodyPrice
      , ("lowerBound",) . BS.pack . show <$> createOrderBodyLowerBound
      , ("upperBound",) . BS.pack . show <$> createOrderBodyUpperBound
      , ("stopLoss",) . BS.pack . show <$> createOrderBodyStopLoss
      , ("takeProfit",) . BS.pack . show <$> createOrderBodyTakeProfit
      , ("trailingStop",) . BS.pack . show <$> createOrderBodyTrailingStop
      ]
  request <- constructRequest od url []
  jsonResponse $ request & setRequestBodyURLEncoded params

data CreateOrderBody
  = CreateOrderBody
  { createOrderBodyInstrument :: InstrumentText
  , createOrderBodyUnits :: Integer
  , createOrderBodySide :: Side
  , createOrderBodyType :: OANDAOrderType
  , createOrderBodyExpiry :: Maybe ZonedTime
    -- ^ Required If order type is ‘limit’, ‘stop’, or ‘marketIfTouched’
  , createOrderBodyPrice :: Maybe Decimal
    -- ^ Required If order type is ‘limit’, ‘stop’, or ‘marketIfTouched’
  , createOrderBodyLowerBound :: Maybe Decimal
  , createOrderBodyUpperBound :: Maybe Decimal
  , createOrderBodyStopLoss :: Maybe Decimal
  , createOrderBodyTakeProfit :: Maybe Decimal
  , createOrderBodyTrailingStop :: Maybe Decimal
  }
  deriving (Show, Generic)

instance ToJSON CreateOrderBody where
  toJSON = genericToJSON $ jsonOpts "createOrderBody"

instance FromJSON CreateOrderBody where
  parseJSON = genericParseJSON $ jsonOpts "createOrderBody"

createOrderBody
  :: InstrumentText
  -> Integer -- ^ Units
  -> Side
  -> OANDAOrderType
  -> CreateOrderBody
createOrderBody instrument units side type' =
  CreateOrderBody
  { createOrderBodyInstrument = instrument
  , createOrderBodyUnits = units
  , createOrderBodySide = side
  , createOrderBodyType = type'
  , createOrderBodyExpiry = Nothing
  , createOrderBodyPrice = Nothing
  , createOrderBodyLowerBound = Nothing
  , createOrderBodyUpperBound = Nothing
  , createOrderBodyStopLoss = Nothing
  , createOrderBodyTakeProfit = Nothing
  , createOrderBodyTrailingStop = Nothing
  }

data OANDAOrderType
  = OANDALimitOrder
  | OANDAStopOrder
  | OANDAMarketIfTouchedOrder
  | OANDAMarketOrder
  deriving (Eq, Generic)

oandaOrderTypeJSONTagModifier :: String -> String
oandaOrderTypeJSONTagModifier "OANDALimitOrder" = "limit"
oandaOrderTypeJSONTagModifier "OANDAStopOrder" = "stop"
oandaOrderTypeJSONTagModifier "OANDAMarketIfTouchedOrder" = "marketIfTouched"
oandaOrderTypeJSONTagModifier "OANDAMarketOrder" = "market"
oandaOrderTypeJSONTagModifier s = s

instance Show OANDAOrderType where
  show OANDALimitOrder = "limit"
  show OANDAStopOrder = "stop"
  show OANDAMarketIfTouchedOrder = "marketIfTouched"
  show OANDAMarketOrder = "market"

instance ToJSON OANDAOrderType where
  toJSON = genericToJSON $ defaultOptions { constructorTagModifier = oandaOrderTypeJSONTagModifier }

instance FromJSON OANDAOrderType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = oandaOrderTypeJSONTagModifier }
