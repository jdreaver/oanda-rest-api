{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility functions.

module OANDA.Util
       ( constructOpts
       , baseURL
       , makeParams
       , commaList
       , jsonOpts
       , jsonResponse
       , jsonResponseArray
       , jsonDelete
       , formatTimeRFC3339
       ) where


#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative, pure)
import           System.Locale (defaultTimeLocale)
#endif

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson.TH as TH
import           Data.ByteString (append)
import           Data.Char (toLower)
import           Data.Decimal
import qualified Data.Map as Map
import           Data.Monoid (
#if !MIN_VERSION_base(4,8,0)
  mempty,
#endif
  (<>))
import           Data.Scientific
import           Data.Text (Text, intercalate, unpack)
import           Data.Time
import           Network.Wreq

import           OANDA.Types

-- | Convenience wrapper around `apiEndpoint`.
baseURL :: OandaData -> String
baseURL (OandaData api _) = apiEndpoint api

-- | Create options for Wreq `getWith` using access token and params.
constructOpts :: OandaData -> [(Text, [Text])] -> Options
constructOpts (OandaData _ t) = constructOpts' t

constructOpts' :: AccessToken -> [(Text, [Text])] -> Options
constructOpts' (AccessToken t) ps = defaults & params' & header'
  where params' = makeParams ps
        header' = header "Authorization" .~ ["Bearer " `append` t]


-- | Create a valid list of params for wreq.
makeParams :: [(Text, [Text])] -> Options -> Options
makeParams xs = params .~ params'
  where params' = [(name, commaList p) | (name, p) <- xs]


-- | Convert a Maybe [Text] item into empty text or comma-separated text.
commaList :: [Text] -> Text
commaList = intercalate ","


-- | Used to derive FromJSON instances.
jsonOpts :: String -> TH.Options
jsonOpts s = TH.defaultOptions { TH.fieldLabelModifier = firstLower . drop (length s) }
  where firstLower [] = []
        firstLower (x:xs) = toLower x : xs

-- | Boilerplate function to perform a request and extract the response body.
jsonResponse :: (FromJSON a) => String -> Options -> IO a
jsonResponse url opts =
  do r <- asJSON =<< getWith opts url
     return $ r ^. responseBody

-- | Boilerplate function to perform a request and extract the response body.
jsonResponseArray :: (FromJSON a) => String -> Options -> String -> IO a
jsonResponseArray url opts name =
  do body <- jsonResponse url opts
     return $ body Map.! (name :: String)

jsonDelete :: (FromJSON a) => String -> Options -> IO a
jsonDelete url opts =
  do r <- asJSON =<< deleteWith opts url
     return $ r ^. responseBody

-- | Formats time according to RFC3339 (which is the time format used by
-- OANDA). Taken from the <https://github.com/HugoDaniel/timerep timerep> library.
formatTimeRFC3339 :: ZonedTime -> String
formatTimeRFC3339 zt@(ZonedTime _ z) = formatTime defaultTimeLocale "%FT%T" zt <> printZone
  where timeZoneStr = timeZoneOffsetString z
        printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr <> ":" <> drop 3 timeZoneStr


instance (Integral a) => FromJSON (DecimalRaw a) where
  parseJSON (Number n) = readDecimalJSON n
  parseJSON (String s) = readDecimalJSON (read (unpack s))
  parseJSON _          = mempty


readDecimalJSON :: (Num i, Applicative f) => Scientific -> f (DecimalRaw i)
readDecimalJSON n = pure $ Decimal ((*) (-1) $ fromIntegral $ base10Exponent n)
                                    (fromIntegral $ coefficient n)
