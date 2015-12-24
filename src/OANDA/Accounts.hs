{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/accounts/ Accounts> section of the
-- API.

module OANDA.Accounts
       ( Account (..)
       , accounts
       ) where

import           Data.Aeson
import qualified Data.Vector as V
import           GHC.Generics (Generic)

import           OANDA.Util
import           OANDA.Types


-- | Wraps the JSON response for accounts
data Account = Account { accountAccountId :: Int
                       , accountAccountName :: String
                       , accountAccountCurrency :: String
                       , accountMarginRate :: Double
                       } deriving (Show, Generic)

instance FromJSON Account where
  parseJSON = genericParseJSON $ jsonOpts "account"

-- | Get all accounts for given access token
accounts :: APIType -> AccessToken -> IO (V.Vector Account)
accounts apit t = do
  let url = apiEndpoint apit ++ "/v1/accounts"
      opts = constructOpts t []
  jsonResponse url opts "accounts"
