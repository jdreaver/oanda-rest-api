{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Internal module for dealing with requests via wreq

module OANDA.Internal.Request
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

import qualified Data.Aeson.TH as TH
import qualified Data.ByteString as BS
import qualified Data.Map as Map

import OANDA.Internal.Import
import OANDA.Internal.Types


-- | Specifies the endpoints for each `APIType`. These are the base URLs for
-- each API call.
baseURL :: OandaEnv -> String
baseURL env = apiEndpoint (apiType env)
  where apiEndpoint Sandbox  = "http://api-sandbox.oanda.com"
        apiEndpoint Practice = "https://api-fxpractice.oanda.com"
        apiEndpoint Live     = "https://api-fxtrade.oanda.com"

-- | Create options for Wreq `getWith` using access token and params.
constructOpts :: OandaEnv -> [(Text, Maybe [Text])] -> Options
constructOpts env = constructOpts' (accessToken env)

constructOpts' :: Maybe AccessToken -> [(Text, Maybe [Text])] -> Options
constructOpts' maybeTok ps = defaults & params' & header'
  where params' = makeParams ps
        header' = maybe id makeHeader maybeTok
        makeHeader (AccessToken t) = header "Authorization" .~ ["Bearer " `BS.append` t]


-- | Create a valid list of params for wreq.
makeParams :: [(Text, Maybe [Text])] -> Options -> Options
makeParams xs = params .~ params'
  where paramToMaybe (name, Just xs') = Just (name, commaList xs')
        paramToMaybe (_, Nothing) = Nothing
        params' = catMaybes $ fmap paramToMaybe xs


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
