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

import           Data.Aeson
import           Data.Decimal
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types


-- | Wraps the JSON response for accounts
data Account = Account { accountAccountId       :: Int
                       , accountAccountName     :: String
                       , accountAccountCurrency :: String
                       , accountMarginRate      :: Decimal
                       } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON = genericParseJSON $ jsonOpts "account"

-- | Get all accounts for given access token
accounts :: OandaData -> IO (V.Vector Account)
accounts od = do
  let url = baseURL od ++ "/v1/accounts"
      opts = constructOpts od []
  jsonResponseArray url opts "accounts"


-- | Get all account info associated with an account ID.
accountInfo :: OandaData -> AccountID -> IO AccountInfo
accountInfo od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid
         opts = constructOpts od []
     jsonResponse url opts


data AccountInfo = AccountInfo
  { accountInfoAccountId       :: Integer
  , accountInfoAccountName     :: String
  , accountInfoBalance         :: Decimal
  , accountInfoUnrealizedPl    :: Decimal
  , accountInfoRealizedPl      :: Decimal
  , accountInfoMarginUsed      :: Decimal
  , accountInfoMarginAvail     :: Decimal
  , accountInfoOpenTrades      :: Integer
  , accountInfoOpenOrders      :: Integer
  , accountInfoMarginRate      :: Decimal
  , accountInfoAccountCurrency :: String
  } deriving (Show, Generic)

instance FromJSON AccountInfo where
  parseJSON = genericParseJSON $ jsonOpts "accountInfo"
