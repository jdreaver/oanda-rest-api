-- | Defines types used in the REST API

module OANDA.Types
       ( APIType (..)
       , apiEndpoint
       , AccessToken (..)
       , AccountID (..)
       ) where

import Data.ByteString (ByteString)

-- | The three endpoint types used in the REST API. See the following link for
-- details: <http://developer.oanda.com/rest-live/development-guide/>
data APIType = Sandbox | Practice | Live

-- | Specifies the endpoints for each `APIType`. These are the base URLs for
-- each API call.
apiEndpoint :: APIType -> String
apiEndpoint Sandbox  = "http://api-sandbox.oanda.com"
apiEndpoint Practice = "https://api-fxpractice.oanda.com"
apiEndpoint Live     = "https://api-fxtrade.oanda.com"

-- | The token given by OANDA used to access the API
newtype AccessToken = AccessToken { unAccessToken :: ByteString }


-- | Integer representing the Account ID of an account
newtype AccountID = AccountID { unAccountID :: Int }
