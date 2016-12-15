-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/orders-df/ Orders> section of the API.

module OANDA.Orders where

import Data.List (intercalate)

import OANDA.Instrument
import OANDA.Internal
import OANDA.Transactions

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

data OrderRequest
  = OrderRequest
  { _orderRequestType :: OrderType
  , _orderRequestClientExtensions :: Maybe ClientExtensions
  , _orderRequestInstrument :: Maybe InstrumentName
  , _orderRequestUnits :: Maybe Decimal
  , _orderRequestTimeInForce :: Maybe TimeInForce
  , _orderRequestPrice :: Maybe PriceValue
  , _orderRequestPriceBound :: Maybe PriceValue
  , _orderRequestPositionFill :: Maybe OrderPositionFill
  , _orderRequestTradeID :: Maybe TradeID
  , _orderRequestClientTradeID :: Maybe Text
  , _orderRequestDistance :: Maybe PriceValue
  , _orderRequestTakeProfitOnFill :: Maybe TakeProfitDetails
  , _orderRequestStopLossOnFill :: Maybe StopLossDetails
  , _orderRequestTrailingStopLossOnFill :: Maybe TrailingStopLossDetails
  , _orderRequestTradeClientExtensions :: Maybe ClientExtensions
  , _orderRequestGtdTime :: Maybe ZonedTime
  } deriving (Show)

makeLenses ''OrderRequest

orderRequest :: OrderType -> OrderRequest
orderRequest orderType =
  OrderRequest
  { _orderRequestType = orderType
  , _orderRequestClientExtensions = Nothing
  , _orderRequestInstrument = Nothing
  , _orderRequestUnits = Nothing
  , _orderRequestTimeInForce = Nothing
  , _orderRequestPrice = Nothing
  , _orderRequestPriceBound = Nothing
  , _orderRequestPositionFill = Nothing
  , _orderRequestTradeID = Nothing
  , _orderRequestClientTradeID = Nothing
  , _orderRequestDistance = Nothing
  , _orderRequestTakeProfitOnFill = Nothing
  , _orderRequestStopLossOnFill = Nothing
  , _orderRequestTrailingStopLossOnFill = Nothing
  , _orderRequestTradeClientExtensions = Nothing
  , _orderRequestGtdTime = Nothing
  }

deriveJSON (unPrefix "_orderRequest") ''OrderRequest

data CreateOrderResponse
  = CreateOrderResponse
  { createOrderResponseOrderCreateTransaction :: Transaction
  , createOrderResponseOrderFillTransaction :: Maybe Transaction
  , createOrderResponseOrderCancelTransaction :: Maybe Transaction
  , createOrderResponseOrderReissueTransaction :: Maybe Transaction
  , createOrderResponseOrderReissueRejectTransaction :: Maybe Transaction
  , createOrderResponseRelatedTransactionIDs :: [TransactionID]
  , createOrderResponseLastTransactionID :: TransactionID
  } deriving (Show)

deriveJSON (unPrefix "createOrderResponse") ''CreateOrderResponse

oandaCreateOrder :: OandaEnv -> AccountID -> OrderRequest -> OANDARequest CreateOrderResponse
oandaCreateOrder env (AccountID accountId) orderRequest' = OANDARequest request
  where
    request =
      baseApiRequest env "POST" ("/v3/accounts/" ++ accountId ++ "/orders")
      & setRequestBodyJSON (object ["order" .= orderRequest'])
