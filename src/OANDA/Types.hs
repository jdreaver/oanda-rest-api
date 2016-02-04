{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines types used in the REST API

module OANDA.Types
       ( OandaEnv
       , apiType
       , accessToken
       , sandboxAuth
       , practiceAuth
       , liveAuth
       , APIType (..)
       , apiEndpoint
       , AccessToken (..)
       , AccountID (..)
       , Side (..)
       , InstrumentText
       ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (pure)
#endif

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Text (unpack)
import           GHC.Generics (Generic)


-- | Wraps an `APIType` and an `AccessToken`. Mainly just a convenience wrapper
-- to make functions have fewer arguments. To instantiate this type, use the
-- `sandboxAuth`, `practiceAuth`, or `liveAuth` functions.
data OandaEnv = OandaEnv
  { apiType     :: APIType
  , accessToken :: Maybe AccessToken
  } deriving (Show)

-- | Use the sandbox API.
sandboxAuth :: OandaEnv
sandboxAuth = OandaEnv Sandbox Nothing

-- | Use the practice API.
practiceAuth :: AccessToken -> OandaEnv
practiceAuth = OandaEnv Practice . Just

-- | Use the live API.
liveAuth :: AccessToken -> OandaEnv
liveAuth = OandaEnv Live . Just


-- | The three endpoint types used in the REST API. See the following link for
-- details: <http://developer.oanda.com/rest-live/development-guide/>
data APIType = Sandbox
             | Practice
             | Live
             deriving (Show)

-- | Specifies the endpoints for each `APIType`. These are the base URLs for
-- each API call.
apiEndpoint :: APIType -> String
apiEndpoint Sandbox  = "http://api-sandbox.oanda.com"
apiEndpoint Practice = "https://api-fxpractice.oanda.com"
apiEndpoint Live     = "https://api-fxtrade.oanda.com"

-- | The token given by OANDA used to access the API
newtype AccessToken = AccessToken { unAccessToken :: ByteString }
                      deriving (Show)


-- | Integer representing the Account ID of an account
newtype AccountID = AccountID { unAccountID :: Int}
                    deriving (Show)

-- | Used when reporting a position in the API
data Side = Buy
          | Sell
          deriving (Show, Generic)

instance FromJSON Side where
  parseJSON (String s) = fmap readSide (pure $ unpack s)
  parseJSON _          = mzero


readSide :: String -> Side
readSide "buy"  = Buy
readSide "sell" = Sell
readSide _      = error "No parse Side"

type InstrumentText = String
