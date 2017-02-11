-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/account-ep/ Account> section of the
-- API.

module OANDA.Accounts
  ( AccountProperties (..)
  , oandaAccounts
  , AccountsResponse (..)
  , oandaAccountDetails
  , AccountDetailsResponse (..)
  , Account (..)
  , Position (..)
  , PositionSide (..)
  ) where

import qualified Data.Vector as V

import OANDA.Instrument
import OANDA.Internal
import OANDA.Orders
import OANDA.Transactions

-- | Wraps the JSON response for accounts
data AccountProperties = AccountProperties
  { accountPropertiesId :: AccountID
  , accountPropertiesMt4AccountID :: Maybe Text
  , accountPropertiesTags :: [Text]
  } deriving (Show)

deriveJSON (unPrefix "accountProperties") ''AccountProperties

oandaAccounts :: OandaEnv -> OANDARequest AccountsResponse
oandaAccounts env = OANDARequest $ baseApiRequest env "GET" "/v3/accounts"

data AccountsResponse
  = AccountsResponse
  { accountsResponseAccounts :: V.Vector AccountProperties
  } deriving (Show)

deriveJSON (unPrefix "accountsResponse") ''AccountsResponse

data PositionSide =
  PositionSide
  { positionSideUnits :: Decimal
  , positionSideAveragePrice :: Maybe PriceValue
  , positionSideTradeIDs :: Maybe [TradeID]
  , positionSidePl :: AccountUnits
  , positionSideUnrealizedPL :: AccountUnits
  , positionSideResettablePL :: AccountUnits
  } deriving (Show)

deriveJSON (unPrefix "positionSide") ''PositionSide

data Position =
  Position
  { positionInstrument :: InstrumentName
  , positionPl :: AccountUnits
  , positionUnrealizedPL :: AccountUnits
  , positionResettablePL :: AccountUnits
  , positionLong :: PositionSide
  , positionShort :: PositionSide
  } deriving (Show)

deriveJSON (unPrefix "position") ''Position

data Account =
  Account
  { accountId :: AccountID
  , accountAlias :: Text
  , accountCurrency :: Currency
  , accountBalance :: AccountUnits
  , accountCreatedByUserID :: Integer
  , accountCreatedTime :: ZonedTime
  , accountPl :: AccountUnits
  , accountResettablePL :: AccountUnits
  , accountResettablePLTime :: Maybe ZonedTime
  , accountMarginRate :: Decimal
  , accountMarginCallEnterTime :: Maybe ZonedTime
  , accountMarginCallExtensionCount :: Maybe Integer
  , accountLastMarginCallExtensionTime :: Maybe ZonedTime
  , accountOpenTradeCount :: Integer
  , accountOpenPositionCount :: Integer
  , accountPendingOrderCount :: Integer
  , accountHedgingEnabled :: Bool
  , accountUnrealizedPL :: AccountUnits
  -- TODO: accountNAV :: AccountUnits
  , accountMarginUsed :: AccountUnits
  , accountMarginAvailable :: AccountUnits
  , accountPositionValue :: AccountUnits
  , accountMarginCloseoutUnrealizedPL :: AccountUnits
  , accountMarginCloseoutNAV :: AccountUnits
  , accountMarginCloseoutMarginUsed :: AccountUnits
  , accountMarginCloseoutPercent :: Decimal
  , accountWithdrawalLimit :: AccountUnits
  , accountMarginCallMarginUsed :: AccountUnits
  , accountMarginCallPercent :: Decimal
  , accountLastTransactionID :: TransactionID
  -- TODO: accountTrades :: [TradeSummary]
  , accountPositions :: [Position]
  , accountOrders :: [Order]
  } deriving (Show)

deriveJSON (unPrefix "account") ''Account

data AccountDetailsResponse =
  AccountDetailsResponse
  { accountDetailsResponseAccount :: Account
  , accountDetailsResponseLastTransactionID :: TransactionID
  } deriving (Show)

deriveJSON (unPrefix "accountDetailsResponse") ''AccountDetailsResponse

oandaAccountDetails :: OandaEnv -> AccountID -> OANDARequest AccountDetailsResponse
oandaAccountDetails env (AccountID accountId) = OANDARequest request
  where
    request =
      baseApiRequest env "GET" ("/v3/accounts/" ++ accountId)

-- TODO:
-- GET /v3/accounts/{AccoundId}/summary
-- GET /v3/accounts/{AccoundId}/instruments
-- PATCH /v3/accounts/{AccoundId}/configuration
-- GET /v3/accounts/{AccoundId}/changes
