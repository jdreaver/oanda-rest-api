{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Internal module for dealing with requests via http-conduit

module OANDA.Internal.Request
  ( OANDARequest (..)
  , makeOandaRequest
  , OANDAStreamingRequest (..)
  , makeOandaStreamingRequest
  , baseApiRequest
  , baseStreamingRequest
  , apiBaseURL
  , streamingBaseURL
  , formatTimeRFC3339
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Network.HTTP.Client as H

import OANDA.Internal.Import
import OANDA.Internal.Types

-- | This is the type returned by the API functions. This is meant to be used
-- with some of our request functions, depending on how safe the user wants to
-- be.
newtype OANDARequest a = OANDARequest { unOANDARequest :: Request }
  deriving (Show)

-- | Simplest way to make requests, but throws exception on errors.
makeOandaRequest :: (MonadIO m, FromJSON a) => OANDARequest a -> m a
makeOandaRequest (OANDARequest request) = getResponseBody <$> httpJSON request

-- | This is the type returned by the streaming API functions. This is meant to
-- be used with some of our streaming request functions, depending on how safe
-- the user wants to be.
newtype OANDAStreamingRequest a = OANDAStreamingRequest { unOANDAStreamingRequest :: Request }
  deriving (Show)

-- | Simplest way to make streaming, but throws exception on errors.
makeOandaStreamingRequest :: (MonadResource m, FromJSON a) => OANDAStreamingRequest a -> ConduitT () a m ()
makeOandaStreamingRequest (OANDAStreamingRequest request) = httpSource request parseBody
  where
    --parseBody :: (MonadIO m) => Response (Source m ByteString) -> Source m a
    parseBody response = mapOutput (either error id . eitherDecodeStrict) $ getResponseBody response

-- | Specifies the endpoints for each `APIType`. These are the base URLs for
-- each API call.
apiBaseURL :: OandaEnv -> String
apiBaseURL env = apiEndpoint (apiType env)
  where
    apiEndpoint Practice = "https://api-fxpractice.oanda.com"
    apiEndpoint Live     = "https://api-fxtrade.oanda.com"

-- | Specifies the streaming endpoints for each `APIType`. These are the base
-- URLs for each streaming call.
streamingBaseURL :: OandaEnv -> String
streamingBaseURL env = apiEndpoint (apiType env)
  where
    apiEndpoint Practice = "https://stream-fxpractice.oanda.com"
    apiEndpoint Live     = "https://stream-fxtrade.oanda.com"

-- | Creates a request with the needed base url and an Authorization header for
-- the Bearer token.
baseRequest :: OandaEnv -> String -> String -> String -> Request
baseRequest env baseUrl requestType url =
  unsafeParseRequest (requestType ++ " " ++ baseUrl ++ url)
  & makeAuthHeader (accessToken env)
  where
    makeAuthHeader (AccessToken t) = addRequestHeader "Authorization" ("Bearer " `BS.append` t)

baseApiRequest :: OandaEnv -> String -> String -> Request
baseApiRequest env = baseRequest env (apiBaseURL env)

baseStreamingRequest :: OandaEnv -> String -> String -> Request
baseStreamingRequest env = baseRequest env (streamingBaseURL env)

unsafeParseRequest :: String -> Request
unsafeParseRequest = unsafeParseRequest' . H.parseUrlThrow
  where
    unsafeParseRequest' (Left err) = error $ show err
    unsafeParseRequest' (Right request) = request

-- | Formats time according to RFC3339 (which is the time format used by
-- OANDA). Taken from the <https://github.com/HugoDaniel/timerep timerep> library.
formatTimeRFC3339 :: ZonedTime -> String
formatTimeRFC3339 zt@(ZonedTime _ z) = formatTime defaultTimeLocale "%FT%T" zt <> printZone
  where timeZoneStr = timeZoneOffsetString z
        printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr <> ":" <> drop 3 timeZoneStr

instance (Show a, Integral a) => ToJSON (DecimalRaw a) where
  toJSON = toJSON . show

instance (Integral a) => FromJSON (DecimalRaw a) where
  parseJSON (Number n) = readDecimalJSON n
  parseJSON (String s) = readDecimalJSON (read (unpack s))
  parseJSON _          = mempty


readDecimalJSON :: (Integral i, Applicative f) => Scientific -> f (DecimalRaw i)
readDecimalJSON n = pure $ fromRational $ toRational n
