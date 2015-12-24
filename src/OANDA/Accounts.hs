{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/accounts/ Accounts> section of the
-- API.

module OANDA.Accounts
       ( Account (..)
       , accounts
       ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           Network.Wreq

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

type AccountResponse = IO (Response (Map.Map String (V.Vector Account)))

-- | Get all accounts for given access token
accounts :: APIType -> AccessToken -> IO (V.Vector Account)
accounts apit t = do
  let url = apiEndpoint apit ++ "/v1/accounts"
      opts = constructOpts t []
  r <- asJSON =<< getWith opts url :: AccountResponse
  let body = r ^. responseBody
      as = body Map.! "accounts"
  return as
