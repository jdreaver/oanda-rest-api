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
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types


-- | Wraps the JSON response for accounts
data Account = Account
  { accountAccountId       :: Int
  , accountAccountName     :: T.Text
  , accountAccountCurrency :: T.Text
  , accountMarginRate      :: Decimal
  } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON = genericParseJSON $ jsonOpts "account"

-- | Get all accounts for given access token
accounts :: OandaEnv -> IO (V.Vector Account)
accounts od = do
  let url = baseURL od ++ "/v1/accounts"
      opts = constructOpts od []
  jsonResponseArray url opts "accounts"


-- | Get all account info associated with an account ID.
accountInfo :: OandaEnv -> AccountID -> IO AccountInfo
accountInfo od (AccountID aid) =
  do let url = baseURL od ++ "/v1/accounts/" ++ show aid
         opts = constructOpts od []
     jsonResponse url opts


data AccountInfo = AccountInfo
  { accountInfoAccountId       :: Integer
  , accountInfoAccountName     :: T.Text
  , accountInfoBalance         :: Decimal
  , accountInfoUnrealizedPl    :: Decimal
  , accountInfoRealizedPl      :: Decimal
  , accountInfoMarginUsed      :: Decimal
  , accountInfoMarginAvail     :: Decimal
  , accountInfoOpenTrades      :: Integer
  , accountInfoOpenOrders      :: Integer
  , accountInfoMarginRate      :: Decimal
  , accountInfoAccountCurrency :: T.Text
  } deriving (Show, Generic)

instance FromJSON AccountInfo where
  parseJSON = genericParseJSON $ jsonOpts "accountInfo"
