{-# LANGUAGE LambdaCase #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live-v20/instrument-ep/ Instrument> section of the
-- API.

module OANDA.Instrument
  ( CandlestickGranularity (..)
  , granularityFromDiffTime
  , granularityToDiffTime
  , WeeklyAlignment (..)
  , PriceValue (..)
  , Candlestick (..)
  , CandlestickData (..)
  , CandlestickArgs (..)
  , candlestickArgsInstrument
  , candlestickArgsPrice
  , candlestickArgsGranularity
  , candlestickArgsCount
  , candlestickArgsFrom
  , candlestickArgsTo
  , candlestickArgsSmooth
  , candlestickArgsIncludeFirst
  , candlestickArgsDailyAlignment
  , candlestickArgsAlignmentTimezone
  , candlestickArgsWeeklyAlignment
  , candlestickArgs
  , oandaCandles
  , CandlestickResponse (..)
  ) where

import OANDA.Internal

data CandlestickGranularity
  = S5
  | S10
  | S15
  | S30
  | M1
  | M2
  | M4
  | M5
  | M10
  | M15
  | M30
  | H1
  | H2
  | H3
  | H4
  | H6
  | H8
  | H12
  | D
  | W
  | M
  deriving (Show)

deriveJSON defaultOptions ''CandlestickGranularity

granularityFromDiffTime :: NominalDiffTime -> Maybe CandlestickGranularity
granularityFromDiffTime =
  \case
    5 -> Just S5
    10 -> Just S10
    15 -> Just S15
    30 -> Just S30
    60 -> Just M1
    120 -> Just M2
    240 -> Just M4
    300 -> Just M5
    600 -> Just M10
    900 -> Just M15
    1800 -> Just M30
    3600 -> Just H1
    7200 -> Just H2
    10800 -> Just H3
    14400 -> Just H4
    21600 -> Just H6
    28800 -> Just H8
    43200 -> Just H12
    86400 -> Just D
    604800 -> Just W
    -- _ -> Just M  -- Not well-defined for a month
    _ -> Nothing

-- | Utility function to convert Granularity to NominalDiffTime. __NOTE__: The
-- conversion from month to NominalDiffTime is not correct in general; we just
-- assume 31 days in a month, which is obviously false for 5 months of the
-- year.
granularityToDiffTime :: CandlestickGranularity -> NominalDiffTime
granularityToDiffTime S5 = 5
granularityToDiffTime S10 = 10
granularityToDiffTime S15 = 15
granularityToDiffTime S30 = 30
granularityToDiffTime M1 = 1 * 60
granularityToDiffTime M2 = 2 * 60
granularityToDiffTime M4 = 4 * 60
granularityToDiffTime M5 = 5 * 60
granularityToDiffTime M10 = 10 * 60
granularityToDiffTime M15 = 15 * 60
granularityToDiffTime M30 = 30 * 60
granularityToDiffTime H1 = 1 * 60 * 60
granularityToDiffTime H2 = 2 * 60 * 60
granularityToDiffTime H3 = 3 * 60 * 60
granularityToDiffTime H4 = 4 * 60 * 60
granularityToDiffTime H6 = 6 * 60 * 60
granularityToDiffTime H8 = 8 * 60 * 60
granularityToDiffTime H12 = 12 * 60 * 60
granularityToDiffTime D = 1 * 60 * 60 * 24
granularityToDiffTime W = 7 * 60 * 60 * 24
granularityToDiffTime M = 31 * 60 * 60 * 24

data WeeklyAlignment
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

newtype PriceValue = PriceValue { unPriceValue :: Text }
  deriving (Show, ToJSON, FromJSON)

data CandlestickData
  = CandlestickData
  { candlestickDataO :: PriceValue
  , candlestickDataH :: PriceValue
  , candlestickDataL :: PriceValue
  , candlestickDataC :: PriceValue
  } deriving (Show)

deriveJSON (unPrefix "candlestickData") ''CandlestickData

data Candlestick
  = Candlestick
  { candlestickTime :: OandaZonedTime
  , candlestickBid :: Maybe CandlestickData
  , candlestickAsk :: Maybe CandlestickData
  , candlestickMid :: Maybe CandlestickData
  , candlestickVolume :: Integer
  , candlestickComplete :: Bool
  } deriving (Show)

deriveJSON (unPrefix "candlestick") ''Candlestick

data CandlestickArgs
  = CandlestickArgs
  { _candlestickArgsInstrument :: InstrumentName
  , _candlestickArgsPrice :: Maybe Text
  , _candlestickArgsGranularity :: CandlestickGranularity
  , _candlestickArgsCount :: Maybe Int
  , _candlestickArgsFrom :: Maybe ZonedTime
  , _candlestickArgsTo :: Maybe ZonedTime
  , _candlestickArgsSmooth :: Maybe Bool
  , _candlestickArgsIncludeFirst :: Maybe Bool
  , _candlestickArgsDailyAlignment :: Maybe Int
  , _candlestickArgsAlignmentTimezone :: Maybe String
  , _candlestickArgsWeeklyAlignment :: Maybe WeeklyAlignment
  } deriving (Show)

candlestickArgs :: InstrumentName -> CandlestickGranularity -> CandlestickArgs
candlestickArgs instrument granularity =
  CandlestickArgs
  { _candlestickArgsInstrument = instrument
  , _candlestickArgsPrice = Nothing
  , _candlestickArgsGranularity = granularity
  , _candlestickArgsCount = Nothing
  , _candlestickArgsFrom = Nothing
  , _candlestickArgsTo = Nothing
  , _candlestickArgsSmooth = Nothing
  , _candlestickArgsIncludeFirst = Nothing
  , _candlestickArgsDailyAlignment = Nothing
  , _candlestickArgsAlignmentTimezone = Nothing
  , _candlestickArgsWeeklyAlignment = Nothing
  }

makeLenses ''CandlestickArgs

oandaCandles :: OandaEnv -> CandlestickArgs -> OANDARequest CandlestickResponse
oandaCandles env CandlestickArgs{..} = OANDARequest request
  where
    instrumentText = unpack $ unInstrumentName _candlestickArgsInstrument
    request =
      baseApiRequest env "GET" ("/v3/instruments/" ++ instrumentText ++ "/candles")
      & setRequestQueryString params
    params =
      catMaybes
      [ ("price",) . Just . encodeUtf8 <$> _candlestickArgsPrice
      , Just ("granularity", Just . fromString $ show _candlestickArgsGranularity)
      , ("count",) . Just . fromString . show <$> _candlestickArgsCount
      , ("from",) . Just . fromString . formatTimeRFC3339 <$> _candlestickArgsFrom
      , ("to",) . Just . fromString . formatTimeRFC3339 <$> _candlestickArgsTo
      , ("smooth",) . Just . fromString . show <$> _candlestickArgsSmooth
      , ("includeFirst",) . Just . fromString . show <$> _candlestickArgsIncludeFirst
      , ("dailyAlignment",) . Just . fromString . show <$> _candlestickArgsDailyAlignment
      , ("alignmentTimezone",) . Just . fromString . show <$> _candlestickArgsAlignmentTimezone
      , ("weeklyAlignment",) . Just . fromString . show <$> _candlestickArgsWeeklyAlignment
      ]

data CandlestickResponse
  = CandlestickResponse
  { candlestickResponseInstrument :: InstrumentName
  , candlestickResponseGranularity :: CandlestickGranularity
  , candlestickResponseCandles :: [Candlestick]
  } deriving (Show)

deriveJSON (unPrefix "candlestickResponse") ''CandlestickResponse
