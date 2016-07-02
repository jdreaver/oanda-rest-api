{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines types used in the REST API

module OANDA.Internal.Types
       ( OandaEnv
       , apiType
       , accessToken
       , sandboxAuth
       , practiceAuth
       , liveAuth
       , APIType (..)
       , AccessToken (..)
       , AccountID (..)
       , Side (..)
       , InstrumentText
       ) where

import qualified Data.ByteString as BS
import OANDA.Internal.Import

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

-- | The token given by OANDA used to access the API
newtype AccessToken = AccessToken { unAccessToken :: BS.ByteString }
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

type InstrumentText = Text
