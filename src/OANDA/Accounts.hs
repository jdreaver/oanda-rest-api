-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/account-ep/ Account> section of the
-- API.

module OANDA.Accounts
  ( AccountProperties (..)
  , oandaAccounts
  ) where

import qualified Data.Vector as V

import OANDA.Internal

-- | Wraps the JSON response for accounts
data AccountProperties = AccountProperties
  { accountPropertiesId :: AccountID
  , accountPropertiesMt4AccountID :: Maybe Text
  , accountPropertiesTags :: [Text]
  } deriving (Show, Generic)

deriveJSON (unPrefix "accountProperties") ''AccountProperties

oandaAccounts :: OandaEnv -> OANDARequest (V.Vector AccountProperties)
oandaAccounts env = OANDARequest request (JsonArrayRequest "accounts")
  where
    request =
      baseRequest env "GET" "/v3/accounts"

-- TODO:
-- GET /v3/accounts/{AccoundId}
-- GET /v3/accounts/{AccoundId}/summary
-- GET /v3/accounts/{AccoundId}/instruments
-- PATCH /v3/accounts/{AccoundId}/configuration
-- GET /v3/accounts/{AccoundId}/changes
