-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/account-ep/ Account> section of the
-- API.

module OANDA.Accounts
  ( AccountProperties (..)
  , oandaAccounts
  , AccountsResponse (..)
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

oandaAccounts :: OandaEnv -> OANDARequest AccountsResponse
oandaAccounts env = OANDARequest $ baseApiRequest env "GET" "/v3/accounts"

data AccountsResponse
  = AccountsResponse
  { accountsResponseAccounts :: V.Vector AccountProperties
  } deriving (Show)

deriveJSON (unPrefix "accountsResponse") ''AccountsResponse

-- TODO:
-- GET /v3/accounts/{AccoundId}
-- GET /v3/accounts/{AccoundId}/summary
-- GET /v3/accounts/{AccoundId}/instruments
-- PATCH /v3/accounts/{AccoundId}/configuration
-- GET /v3/accounts/{AccoundId}/changes
