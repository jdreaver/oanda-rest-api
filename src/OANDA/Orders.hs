-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/orders-df/ Orders> section of the API.

module OANDA.Orders where

import Data.List (intercalate)

import OANDA.Instrument
import OANDA.Internal hiding (intercalate)

newtype OrderID = OrderID { unOrderID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype TransactionID = TransactionID { unTransactionID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype TradeID = TradeID { unTradeID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

data OrderType
  = MARKET
  | LIMIT
  | STOP
  | MARKET_IF_TOUCHED
  | TAKE_PROFIT
  | STOP_LOSS
  | TRAILING_STOP_LOSS
  deriving (Show, Eq)

deriveJSON defaultOptions ''OrderType

data OrderState
  = PENDING
  | FILLED
  | TRIGGERED
  | CANCELLED
  deriving (Show, Eq)

deriveJSON defaultOptions ''OrderState

data ClientExtensions
  = ClientExtensions
  { clientExtensionsID :: Text
  , clientExtensionsTag :: Text
  , clientExtensionsComment :: Text
  } deriving (Show)

deriveJSON (unPrefix "clientExtensions") ''ClientExtensions

data TimeInForce
  = GTC
  | GTD
  | GFD
  | FOK
  | IOC
  deriving (Show, Eq)

deriveJSON defaultOptions ''TimeInForce

data OrderPositionFill
  = OPEN_ONLY
  | REDUCE_FIRST
  | REDUCE_ONLY
  | POSITION_DEFAULT
  deriving (Show, Eq)

deriveJSON defaultOptions ''OrderPositionFill

data MarketOrderPositionCloseout
  = MarketOrderPositionCloseout
  { marketOrderPositionCloseoutInstrument :: InstrumentName
  , marketOrderPositionCloseoutUnits :: Text
  } deriving (Show)

deriveJSON (unPrefix "marketOrderPositionCloseout") ''MarketOrderPositionCloseout

data MarketOrderTradeClose
  = MarketOrderTradeClose
  { marketOrderTradeCloseTradeID :: TradeID
  , marketOrderTradeCloseClientTradeID :: Text
  , marketOrderTradeCloseUnits :: Text
  } deriving (Show)

deriveJSON (unPrefix "marketOrderTradeClose") ''MarketOrderTradeClose

data MarketOrderMarginCloseout
  = MarketOrderMarginCloseout
  { marketOrderMarginCloseoutReason :: Text
  } deriving (Show)

deriveJSON (unPrefix "marketOrderMarginCloseout") ''MarketOrderMarginCloseout

data MarketOrderDelayedTradeClose
  = MarketOrderDelayedTradeClose
  { marketOrderDelayedTradeCloseTradeID :: TradeID
  , marketOrderDelayedTradeCloseClientTradeID :: Text
  , marketOrderDelayedTradeCloseSourceTransactionID :: TransactionID
  } deriving (Show)

deriveJSON (unPrefix "marketOrderDelayedTradeClose") ''MarketOrderDelayedTradeClose

data TakeProfitDetails
  = TakeProfitDetails
  { takeProfitDetailsPrice :: Text
  , takeProfitDetailsTimeInForce :: TimeInForce
  , takeProfitDetailsGtdTime :: ZonedTime
  , takeProfitDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "takeProfitDetails") ''TakeProfitDetails

data StopLossDetails
  = StopLossDetails
  { stopLossDetailsPrice :: Text
  , stopLossDetailsTimeInForce :: TimeInForce
  , stopLossDetailsGtdTime :: ZonedTime
  , stopLossDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "stopLossDetails") ''StopLossDetails

data TrailingStopLossDetails
  = TrailingStopLossDetails
  { trailingStopLossDetailsDistance :: Text
  , trailingStopLossDetailsTimeInForce :: TimeInForce
  , trailingStopLossDetailsGtdTime :: ZonedTime
  , trailingStopLossDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "trailingStopLossDetails") ''TrailingStopLossDetails

data Order
  = Order
  { orderId :: OrderID
  , orderCreateTime :: ZonedTime
  , orderState :: OrderState
  , orderClientExtensions :: Maybe ClientExtensions
  , orderType :: OrderType
  , orderInstrument :: Maybe InstrumentName
  , orderUnits :: Maybe Decimal
  , orderTimeInForce :: Maybe TimeInForce
  , orderPrice :: Maybe PriceValue
  , orderPriceBound :: Maybe PriceValue
  , orderPositionFill :: Maybe OrderPositionFill
  , orderInitialMarketPrice :: Maybe PriceValue
  , orderTradeClose :: Maybe MarketOrderTradeClose
  , orderTradeID :: Maybe TradeID
  , orderClientTradeID :: Maybe Text
  , orderDistance :: Maybe PriceValue
  , orderLongPositionCloseout :: Maybe MarketOrderPositionCloseout
  , orderShortPositionCloseout :: Maybe MarketOrderPositionCloseout
  , orderMarginCloseout :: Maybe MarketOrderMarginCloseout
  , orderDelayedTradeClose :: Maybe MarketOrderDelayedTradeClose
  , orderTakeProfitOnFill :: Maybe TakeProfitDetails
  , orderStopLossOnFill :: Maybe StopLossDetails
  , orderTrailingStopLossOnFill :: Maybe TrailingStopLossDetails
  , orderTradeClientExtensions :: Maybe ClientExtensions
  , orderFillingTransactionID :: Maybe TransactionID
  , orderFilledTime :: Maybe ZonedTime
  , orderTradeOpenedID :: Maybe TradeID
  , orderTradeReducedID :: Maybe TradeID
  , orderTradeClosedIDs :: Maybe [TradeID]
  , orderCancellingTransactionID :: Maybe TransactionID
  , orderCancelledTime :: Maybe ZonedTime
  , orderGtdTime :: Maybe ZonedTime
  , orderReplacesOrderID :: Maybe OrderID
  , orderReplacedByOrderID :: Maybe OrderID
  } deriving (Show)

deriveJSON (unPrefix "order") ''Order

data OrdersArgs
  = OrdersArgs
  { _ordersArgsIds :: Maybe [OrderID]
  , _ordersArgsState :: Maybe OrderState
  , _ordersArgsInstrument :: Maybe InstrumentName
  , _ordersArgsCount :: Maybe Int
  , _ordersArgsBeforeID :: Maybe OrderID
  } deriving (Show)

ordersArgs :: OrdersArgs
ordersArgs = OrdersArgs Nothing Nothing Nothing Nothing Nothing

makeLenses ''OrdersArgs

data OrdersResponse
  = OrdersResponse
  { ordersResponseOrders :: [Order]
  , ordersResponseLastTransactionID :: TransactionID
  } deriving (Show)

deriveJSON (unPrefix "ordersResponse") ''OrdersResponse

oandaOrders :: OandaEnv -> AccountID -> OrdersArgs -> OANDARequest OrdersResponse
oandaOrders env (AccountID accountId) OrdersArgs{..} = OANDARequest request
  where
    request =
      baseApiRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/orders")
      & setRequestQueryString params
    params =
      catMaybes
      [ ("ids",) . Just . fromString . intercalate "," . fmap (show . unOrderID) <$> _ordersArgsIds
      , ("state",) . Just . fromString . show <$> _ordersArgsState
      , ("instrument",) . Just . fromString . unpack . unInstrumentName <$> _ordersArgsInstrument
      , ("count",) . Just . fromString . show <$> _ordersArgsCount
      , ("beforeID",) . Just . fromString . show . unOrderID <$> _ordersArgsBeforeID
      ]
