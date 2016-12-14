-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/transactions-ep/ Transaction
-- History> section of the API.

module OANDA.Transactions where

import OANDA.Internal

newtype TransactionID = TransactionID { unTransactionID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype TradeID = TradeID { unTradeID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype OrderID = OrderID { unOrderID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

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

data TimeInForce
  = GTC
  | GTD
  | GFD
  | FOK
  | IOC
  deriving (Show, Eq)

deriveJSON defaultOptions ''TimeInForce

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

data ClientExtensions
  = ClientExtensions
  { clientExtensionsID :: Text
  , clientExtensionsTag :: Text
  , clientExtensionsComment :: Text
  } deriving (Show)

deriveJSON (unPrefix "clientExtensions") ''ClientExtensions

data TakeProfitDetails
  = TakeProfitDetails
  { takeProfitDetailsPrice :: Text
  , takeProfitDetailsTimeInForce :: TimeInForce
  , takeProfitDetailsGtdTime :: ZonedTime
  , takeProfitDetailsClientExtensions :: ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "takeProfitDetails") ''TakeProfitDetails

data StopLossDetails
  = StopLossDetails
  { stopLossDetailsPrice :: Text
  , stopLossDetailsTimeInForce :: TimeInForce
  , stopLossDetailsGtdTime :: ZonedTime
  , stopLossDetailsClientExtensions :: ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "stopLossDetails") ''StopLossDetails

data TrailingStopLossDetails
  = TrailingStopLossDetails
  { trailingStopLossDetailsDistance :: Text
  , trailingStopLossDetailsTimeInForce :: TimeInForce
  , trailingStopLossDetailsGtdTime :: ZonedTime
  , trailingStopLossDetailsClientExtensions :: ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "trailingStopLossDetails") ''TrailingStopLossDetails

data TradeOpen
  = TradeOpen
  { tradeOpenTradeId :: TradeID
  , tradeOpenUnits :: Decimal
  , tradeOpenClientExtensions :: ClientExtensions
  } deriving (Show)

deriveJSON (unPrefix "tradeOpen") ''TradeOpen

data TradeReduce
  = TradeReduce
  { tradeReduceTradeId :: TradeID
  , tradeReduceUnits :: Decimal
  , tradeReduceRealizedPL :: AccountUnits
  , tradeReduceFinancing :: AccountUnits
  } deriving (Show)

deriveJSON (unPrefix "tradeReduce") ''TradeReduce

data OpenTradeFinancing
  = OpenTradeFinancing
  { openTradeFinancingTradeId :: TradeID
  , openTradeFinancingFinancing :: AccountUnits
  } deriving (Show)

deriveJSON (unPrefix "openTradeFinancing") ''OpenTradeFinancing

data PositionFinancing
  = PositionFinancing
  { positionFinancingInstrumentID :: InstrumentName
  , positionFinancingFinancing :: AccountUnits
  , positionFinancingOpenTradeFinancings :: [OpenTradeFinancing]
  } deriving (Show)

deriveJSON (unPrefix "positionFinancing") ''PositionFinancing

data Transaction = Transaction
  { -- Common to all transactions
    transactionId :: TransactionID
  , transactionTime :: ZonedTime
  , transactionAccountID :: AccountID
  , transactionUserID :: Integer
  , transactionBatchID :: TransactionID
  , transactionType :: TransactionType

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
  , transactionGtdTime :: Maybe ZonedTime
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
  OANDARequest $ baseRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/transactions/" ++ unpack transId)

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
      baseRequest env "GET" ("/v3/accounts/" ++ accountId ++ "/transactions/sinceid")
      & setRequestQueryString [("id", Just $ encodeUtf8 transId)]
