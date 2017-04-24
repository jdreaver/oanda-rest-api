-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/transactions-ep/ Transaction
-- History> section of the API.

module OANDA.Transactions where

import qualified Data.ByteString.Char8 as BS8

import OANDA.Internal

newtype OrderID = OrderID { unOrderID :: Int }
  deriving (Show, Eq)

instance ToJSON OrderID where
  toJSON = toJSON . show . unOrderID

instance FromJSON OrderID where
  parseJSON = fmap OrderID . parseJSONFromString

newtype TransactionID = TransactionID { unTransactionID :: Int }
  deriving (Show, Eq)

instance ToJSON TransactionID where
  toJSON = toJSON . show . unTransactionID

instance FromJSON TransactionID where
  parseJSON = fmap TransactionID . parseJSONFromString

newtype TradeID = TradeID { unTradeID :: Int }
  deriving (Show, Eq)

instance ToJSON TradeID where
  toJSON = toJSON . show . unTradeID

instance FromJSON TradeID where
  parseJSON = fmap TradeID . parseJSONFromString

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
  , takeProfitDetailsGtdTime :: OandaZonedTime
  , takeProfitDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "takeProfitDetails") ''TakeProfitDetails

data StopLossDetails
  = StopLossDetails
  { stopLossDetailsPrice :: Text
  , stopLossDetailsTimeInForce :: TimeInForce
  , stopLossDetailsGtdTime :: OandaZonedTime
  , stopLossDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "stopLossDetails") ''StopLossDetails

data TrailingStopLossDetails
  = TrailingStopLossDetails
  { trailingStopLossDetailsDistance :: Text
  , trailingStopLossDetailsTimeInForce :: TimeInForce
  , trailingStopLossDetailsGtdTime :: OandaZonedTime
  , trailingStopLossDetailsClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "trailingStopLossDetails") ''TrailingStopLossDetails

data TransactionType
  = CREATE
  | CLOSE
  | REOPEN
  | CLIENT_CONFIGURE
  | CLIENT_CONFIGURE_REJECT
  | TRANSFER_FUNDS
  | TRANSFER_FUNDS_REJECT
  | MARKET_ORDER
  | MARKET_ORDER_REJECT
  | LIMIT_ORDER
  | LIMIT_ORDER_REJECT
  | STOP_ORDER
  | STOP_ORDER_REJECT
  | MARKET_IF_TOUCHED_ORDER
  | MARKET_IF_TOUCHED_ORDER_REJECT
  | TAKE_PROFIT_ORDER
  | TAKE_PROFIT_ORDER_REJECT
  | STOP_LOSS_ORDER
  | STOP_LOSS_ORDER_REJECT
  | TRAILING_STOP_LOSS_ORDER
  | TRAILING_STOP_LOSS_ORDER_REJECT
  | ORDER_FILL
  | ORDER_CANCEL
  | ORDER_CANCEL_REJECT
  | ORDER_CLIENT_EXTENSIONS_MODIFY
  | ORDER_CLIENT_EXTENSIONS_MODIFY_REJECT
  | TRADE_CLIENT_EXTENSIONS_MODIFY
  | TRADE_CLIENT_EXTENSIONS_MODIFY_REJECT
  | MARGIN_CALL_ENTER
  | MARGIN_CALL_EXTEND
  | MARGIN_CALL_EXIT
  | DELAYED_TRADE_CLOSURE
  | DAILY_FINANCING
  | RESET_RESETTABLE_PL
  deriving (Show, Eq)

deriveJSON defaultOptions ''TransactionType

data TradeOpen
  = TradeOpen
  { tradeOpenTradeID :: TradeID
  , tradeOpenUnits :: Decimal
  , tradeOpenClientExtensions :: Maybe ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "tradeOpen") ''TradeOpen

data TradeReduce
  = TradeReduce
  { tradeReduceTradeID :: TradeID
  , tradeReduceUnits :: Decimal
  , tradeReduceRealizedPL :: AccountUnits
  , tradeReduceFinancing :: AccountUnits
  } deriving (Show)

deriveJSON (unPrefix "tradeReduce") ''TradeReduce

data OpenTradeFinancing
  = OpenTradeFinancing
  { openTradeFinancingTradeID :: TradeID
  , openTradeFinancingFinancing :: AccountUnits
  } deriving (Show)

deriveJSON (unPrefix "openTradeFinancing") ''OpenTradeFinancing

data PositionFinancing
  = PositionFinancing
  { -- BUG: Their docs say instrumentID but the field is actually called instrument
    positionFinancingInstrument :: InstrumentName
  , positionFinancingFinancing :: AccountUnits
  , positionFinancingOpenTradeFinancings :: [OpenTradeFinancing]
  } deriving (Show)

deriveJSON (unPrefix "positionFinancing") ''PositionFinancing

data Transaction = Transaction
  { -- Common to all transactions
    transactionId :: Maybe TransactionID
  , transactionTime :: Maybe OandaZonedTime
  , transactionAccountID :: Maybe AccountID
  , transactionUserID :: Maybe Integer
  , transactionBatchID :: Maybe TransactionID
  , transactionType :: Maybe TransactionType

  -- Specific to individual transactions
  , transactionDivisionID :: Maybe Integer
  , transactionSiteID :: Maybe Integer
  , transactionAccountUserID :: Maybe Integer
  , transactionAccountNumber :: Maybe Integer
  , transactionHomeCurrency :: Maybe Currency
  , transactionAlias :: Maybe Text
  , transactionMarginRate :: Maybe Decimal
  , transactionRejectReason :: Maybe Text
  , transactionAmount :: Maybe AccountUnits
  , transactionFundingReason :: Maybe Text
  , transactionAccountBalance :: Maybe AccountUnits
  , transactionInstrument :: Maybe InstrumentText
  , transactionUnits :: Maybe Decimal
  , transactionPrice :: Maybe Decimal
  , transactionTimeInForce :: Maybe TimeInForce
  , transactionPriceBound :: Maybe Text
  , transactionPositionFill :: Maybe Text
  , transactionMarketOrderTradeClose :: Maybe MarketOrderTradeClose
  , transactionLongPositionCloseout :: Maybe MarketOrderPositionCloseout
  , transactionShortPositionCloseout :: Maybe MarketOrderPositionCloseout
  , transactionMarginCloseout :: Maybe MarketOrderMarginCloseout
  , transactionDelayedTradeClose :: Maybe MarketOrderDelayedTradeClose
  , transactionReason :: Maybe Text
  , transactionClientExtensions :: Maybe ClientExtensions
  , transactionTakeProfitOnFill :: Maybe TakeProfitDetails
  , transactionStopLossOnFill :: Maybe StopLossDetails
  , transactionTrailingStopLossOnFill :: Maybe TrailingStopLossDetails
  , transactionTradeClientExtensions :: Maybe ClientExtensions
  , transactionGtdTime :: Maybe OandaZonedTime
  , transactionReplacesOrderID :: Maybe OrderID
  , transactionReplacedOrderCancelTransactionID :: Maybe TransactionID
  , transactionIntendedReplacesOrderID :: Maybe OrderID
  , transactionDistance :: Maybe Text
  , transactionOrderID :: Maybe OrderID
  , transactionClientOrderID :: Maybe Text
  , transactionPl :: Maybe AccountUnits
  , transactionFinancing :: Maybe AccountUnits
  , transactionTradeOpened :: Maybe TradeOpen
  , transactionTradesClosed :: Maybe [TradeReduce]
  , transactionTradeReduced :: Maybe TradeReduce
  , transactionTradeClientExtensionsModify :: Maybe ClientExtensions
  , transactionExtensionNumber :: Maybe Integer
  , transactionTradeIDs :: Maybe TradeID
  , transactionAccountFinancingMode :: Maybe Text
  , transactionPositionFinancings :: Maybe [PositionFinancing]
  } deriving (Show)

deriveJSON (unPrefix "transaction") ''Transaction

oandaTransaction :: OandaEnv -> AccountID -> TransactionID -> OANDARequest Transaction
oandaTransaction env (AccountID accountId) (TransactionID transId) =
  OANDARequest $ baseApiRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/transactions/" ++ show transId)

data TransactionsSinceIDResponse
  = TransactionsSinceIDResponse
  { transactionsSinceIDResponseTransactions :: [Transaction]
  , transactionsSinceIDResponseLastTransactionID :: TransactionID
  } deriving (Show)

deriveJSON (unPrefix "transactionsSinceIDResponse") ''TransactionsSinceIDResponse

oandaTransactionsSinceID :: OandaEnv -> AccountID -> TransactionID -> OANDARequest TransactionsSinceIDResponse
oandaTransactionsSinceID env (AccountID accountId) (TransactionID transId) = OANDARequest request
  where
    request =
      baseApiRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/transactions/sinceid")
      & setRequestQueryString [("id", Just $ BS8.pack $ show transId)]

data TransactionHeartbeat
  = TransactionHeartbeat
  { transactionHeartbeatLastTransactionID :: TransactionID
  , transactionHeartbeatTime :: OandaZonedTime
  } deriving (Show)

deriveJSON (unPrefix "transactionHeartbeat") ''TransactionHeartbeat

data TransactionsStreamResponse
  = StreamTransactionHeartbeat TransactionHeartbeat
  | StreamTransaction Transaction
  deriving (Show)

-- The ToJSON instance is just for debugging, it's not actually correct
deriveToJSON defaultOptions ''TransactionsStreamResponse

instance FromJSON TransactionsStreamResponse where
  parseJSON (Object o) = do
    type' <- o .: "type" :: Parser String
    case type' of
      "HEARTBEAT" -> StreamTransactionHeartbeat <$> parseJSON (Object o)
      _ -> StreamTransaction <$> parseJSON (Object o)
  parseJSON _ = mempty

oandaTransactionStream :: OandaEnv -> AccountID -> OANDAStreamingRequest TransactionsStreamResponse
oandaTransactionStream env (AccountID accountId) =
  OANDAStreamingRequest $ baseStreamingRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/transactions/stream")


-- | Order to modify an existing Trade (i.e. Transaction)
data TradePrice = TradePrice { tradePrice :: Text } 
  deriving (Show)
deriveJSON (unPrefix "trade") ''TradePrice

data TradeTrailingStop = TradeTrailingStop { tradeTrailingStopDistance :: Decimal}
    deriving (Show)
deriveJSON (unPrefix "tradeTrailingStop") ''TradeTrailingStop

data TradeOrder = TradeOrder { tradeOrderTakeProfit    :: Maybe TradePrice
                             , tradeOrderStopLoss      :: Maybe TradePrice
                             , tradeOrderTraillingStop :: Maybe TradeTrailingStop
                             } deriving (Show)
deriveJSON (unPrefix "tradeOrder") ''TradeOrder

data OrderReason = CLIENT_REQUEST_REPLACED
    | REPLACEMENT
    | CLIENT_ORDER
    deriving (Show, Eq)
deriveJSON defaultOptions ''OrderReason

data TransactionTrigger = TRIGGER_DEFAULT 
    deriving (Show, Eq)
deriveJSON defaultOptions ''TransactionTrigger

data OrderTransaction = OrderTransaction
    { orderTransaction_Type                    :: Maybe TransactionType
    , orderTransaction_OrderID                 :: Maybe OrderID
    , orderTransaction_TradeID                 :: Maybe TransactionID
    , orderTransaction_CancellingTransactionID :: Maybe TransactionID
    , orderTransaction_ReplacedByOrderID       :: Maybe OrderID
    , orderTransaction_Reason                  :: Maybe OrderReason
    , orderTransaction_ID                      :: Maybe TransactionID
    , orderTransaction_UserID                  :: Maybe Integer
    , orderTransaction_AccountID               :: Maybe AccountID
    , orderTransaction_BatchID                 :: Maybe TransactionID
    , orderTransaction_TimeInForce             :: Maybe TimeInForce
    , orderTransaction_RequestID               :: Maybe String
    , orderTransaction_Time                    :: Maybe OandaZonedTime
    , orderTransaction_Price                   :: Maybe Text
    , orderTransaction_Distance                :: Maybe Decimal
    } deriving (Show)
deriveJSON (unPrefix "orderTransaction_") ''OrderTransaction

data OrderTransactionResponse =  OrderTransactionResponse
    { orderTransactionResponse_TakeProfitOrderCancelTransaction :: Maybe OrderTransaction
    , orderTransactionResponse_TakeProfitOrderTransaction       :: Maybe OrderTransaction
    , orderTransactionResponse_StopLossOrderTransaction         :: Maybe OrderTransaction
    , orderTransactionResponse_TrailingStopLossOrderTransaction :: Maybe OrderTransaction
    , orderTransactionResponse_RelatedTransactionIDs            :: Maybe [TransactionID]
    , orderTransactionResponse_LastTransactionID                :: Maybe TransactionID
    } deriving (Show)
deriveJSON (unPrefix "orderTransactionResponse_") ''OrderTransactionResponse

oandaModifyTrade :: OandaEnv -> AccountID -> TransactionID -> TradeOrder -> OANDARequest OrderTransactionResponse
oandaModifyTrade env (AccountID accountId) transactionID tradeOrder = OANDARequest request
  where
    request =
      baseApiRequest env "PUT" ("/v3/accounts/" ++ accountId 
                                                ++ "/trades/" 
                                                ++ transactionID' 
                                                ++ "/orders"
                               )
      & setRequestBodyJSON tradeOrder
    transactionID' = show $ unTransactionID transactionID


-- | Cancel an existing transaction
oandaCloseTrade :: OandaEnv -> AccountID -> TransactionID -> OANDARequest OrderTransactionResponse
oandaCloseTrade env (AccountID accountId) transactionID = OANDARequest request
  where
    request =
      baseApiRequest env "PUT" ("/v3/accounts/" ++ accountId 
                                                ++ "/trades/" 
                                                ++ transactionID' 
                                                ++ "/close"
                               )
    transactionID' = show $ unTransactionID transactionID


