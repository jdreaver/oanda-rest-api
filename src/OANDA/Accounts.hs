{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/accounts/ Accounts> section of the
-- API.

module OANDA.Accounts
  ( Account (..)
  , accounts
  , AccountInfo (..)
  , accountInfo
  ) where

import qualified Data.Vector as V

import OANDA.Internal

-- | Wraps the JSON response for accounts
data Account = Account
  { accountAccountId       :: Int
  , accountAccountName     :: Text
  , accountAccountCurrency :: Text
  , accountMarginRate      :: Decimal
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON = genericParseJSON $ jsonOpts "account"

-- | Get all accounts for given access token
accounts :: OandaEnv -> IO (V.Vector Account)
accounts od = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts"
  request <- constructRequest od url []
  jsonResponseArray request "accounts"


-- | Get all account info associated with an account ID.
accountInfo :: OandaEnv -> AccountID -> IO AccountInfo
accountInfo od (AccountID aid) = do
  let url = "GET " ++ baseURL od ++ "/v1/accounts/" ++ show aid
  request <- constructRequest od url []
  jsonResponse request


data AccountInfo = AccountInfo
  { accountInfoAccountId       :: Integer
  , accountInfoAccountName     :: Text
  , accountInfoBalance         :: Decimal
  , accountInfoUnrealizedPl    :: Decimal
  , accountInfoRealizedPl      :: Decimal
  , accountInfoMarginUsed      :: Decimal
  , accountInfoMarginAvail     :: Decimal
  , accountInfoOpenTrades      :: Integer
  , accountInfoOpenOrders      :: Integer
  , accountInfoMarginRate      :: Decimal
  , accountInfoAccountCurrency :: Text
  } deriving (Show, Generic)

instance FromJSON AccountInfo where
  parseJSON = genericParseJSON $ jsonOpts "accountInfo"
