{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines the endpoints listed in the
-- <http://developer.oanda.com/rest-live/rates/ Rates> section of the API.

module OANDA.Rates
  ( InstrumentsArgs (..)
  , instrumentsArgs
  , Instrument (..)
  , instruments
  , Price (..)
  , prices
  , MidpointCandlestick (..)
  , midpointCandles
  , BidAskCandlestick (..)
  , bidaskCandles
  , CandlesArgs (..)
  , candlesArgs
  , CandlesCount (..)
  , DayOfWeek (..)
  , Granularity (..)
  , granularityToDiffTime
  ) where

import qualified Data.Vector as V

import OANDA.Internal


data InstrumentsArgs = InstrumentsArgs
  { instrumentsFields      :: Maybe [Text]
  , instrumentsInstruments :: Maybe [Text]
  } deriving (Show)

instrumentsArgs :: InstrumentsArgs
instrumentsArgs = InstrumentsArgs Nothing Nothing

data Instrument = Instrument
  { instrumentInstrument      :: Text
  , instrumentPip             :: Maybe Decimal
  , instrumentMaxTradeUnits   :: Maybe Integer
  , instrumentDisplayName     :: Maybe Text
  , instrumentPrecision       :: Maybe Decimal
  , instrumentMaxTrailingStop :: Maybe Decimal
  , instrumentMinTrailingStop :: Maybe Decimal
  , instrumentMarginRate      :: Maybe Decimal
  , instrumentHalted          :: Maybe Bool
  , instrumentInterestRate    :: Maybe Object
  } deriving (Show, Generic)

instance FromJSON Instrument where
  parseJSON = genericParseJSON $ jsonOpts "instrument"

-- | Retrieve a list of instruments from OANDA
instruments :: OandaEnv -> AccountID -> InstrumentsArgs -> IO (V.Vector Instrument)
instruments od (AccountID aid) (InstrumentsArgs fs is) = do
  let url = "GET " ++ baseURL od ++ "/v1/instruments"
  request <- constructRequest od url
    [ ("accountId", Just [pack $ show aid])
    , ("instruments", is)
    , ("fields", fs)
    ]
  jsonResponseArray request "instruments"

-- | Retrieve the current prices for a list of instruments.
prices :: OandaEnv -> [InstrumentText] -> Maybe ZonedTime -> IO (V.Vector Price)
prices od is zt = do
  let
    url = "GET " ++ baseURL od ++ "/v1/prices"
    ztOpt = maybe [] (\zt' -> [("since", Just [pack $ formatTimeRFC3339 zt'])]) zt
  request <- constructRequest od url (("instruments", Just is) : ztOpt)
  jsonResponseArray request "prices"

data Price = Price
  { priceInstrument :: InstrumentText
  , priceTime       :: ZonedTime
  , priceBid        :: Decimal
  , priceAsk        :: Decimal
  } deriving (Show, Generic)

instance FromJSON Price where
  parseJSON = genericParseJSON $ jsonOpts "price"


-- | Retrieve the price history of a single instrument in midpoint candles
midpointCandles :: OandaEnv -> InstrumentText -> CandlesArgs ->
                   IO (V.Vector MidpointCandlestick)
midpointCandles od i args = do
  request <- candleOpts od i args "midpoint"
  response <- jsonResponse request :: IO MidpointCandlesResponse
  return $ _midcandlesResponseCandles response

-- | Retrieve the price history of a single instrument in bid/ask candles
bidaskCandles :: OandaEnv -> InstrumentText -> CandlesArgs ->
                 IO (V.Vector BidAskCandlestick)
bidaskCandles od i args = do
  request <- candleOpts od i args "bidask"
  response <- jsonResponse request :: IO BidAskCandlesResponse
  return $ _bidaskResponseCandles response


-- | Utility function for both candle history functions
candleOpts :: OandaEnv -> InstrumentText -> CandlesArgs -> String -> IO Request
candleOpts od i (CandlesArgs c g di atz wa) fmt = constructRequest od url opts
  where url = "GET " ++ baseURL od ++ "/v1/candles"
        opts =
          [ ("instrument", Just [i])
          , ("granularity", (:[]) . pack . show <$> g)
          , ("candleFormat", Just [pack fmt])
          , ("dailyAlignment", (:[]) . pack . show <$> di)
          , ("alignmentTimeZone", (:[]) <$> atz)
          , ("weeklyAlignment", (:[]) . pack . show <$> wa)
          ] ++ maybe [] countOpts c
        countOpts (Count c') = [("count", Just [pack $ show c'])]
        countOpts (StartEnd st ed incf) =
          [ ("start", Just [pack $ formatTimeRFC3339 st])
          , ("end", Just [pack $ formatTimeRFC3339 ed])
          , ("includeFirst", Just [pack $ map toLower (show incf)])
          ]
        countOpts (StartCount st c' incf) =
          [ ("start", Just [pack $ formatTimeRFC3339 st])
          , ("count", Just [pack $ show c'])
          , ("includeFirst", Just [pack $ map toLower (show incf)])
          ]
        countOpts (EndCount ed c') =
          [ ("end", Just [pack $ formatTimeRFC3339 ed])
          , ("count", Just [pack $ show c'])
          ]

data MidpointCandlestick = MidpointCandlestick
  { midpointCandlestickTime     :: ZonedTime
  , midpointCandlestickOpenMid  :: Decimal
  , midpointCandlestickHighMid  :: Decimal
  , midpointCandlestickLowMid   :: Decimal
  , midpointCandlestickCloseMid :: Decimal
  , midpointCandlestickVolume   :: Int
  , midpointCandlestickComplete :: Bool
  } deriving (Show, Generic)

instance FromJSON MidpointCandlestick where
  parseJSON = genericParseJSON $ jsonOpts "midpointCandlestick"


data BidAskCandlestick = BidAskCandlestick
  { bidaskCandlestickTime     :: ZonedTime
  , bidaskCandlestickOpenBid  :: Decimal
  , bidaskCandlestickOpenAsk  :: Decimal
  , bidaskCandlestickHighBid  :: Decimal
  , bidaskCandlestickHighAsk  :: Decimal
  , bidaskCandlestickLowBid   :: Decimal
  , bidaskCandlestickLowAsk   :: Decimal
  , bidaskCandlestickCloseBid :: Decimal
  , bidaskCandlestickCloseAsk :: Decimal
  , bidaskCandlestickVolume   :: Int
  , bidaskCandlestickComplete :: Bool
  } deriving (Show, Generic)

instance FromJSON BidAskCandlestick where
  parseJSON = genericParseJSON $ jsonOpts "bidaskCandlestick"

data CandlesArgs = CandlesArgs
  { candlesCount           :: Maybe CandlesCount
  , candlesGranularity     :: Maybe Granularity
  , candlesDailyAlignment  :: Maybe Int
  , candlesAlignmentTZ     :: Maybe Text
  , candlesWeeklyAlignment :: Maybe DayOfWeek
  } deriving (Show)

candlesArgs :: CandlesArgs
candlesArgs = CandlesArgs Nothing Nothing Nothing Nothing Nothing

data CandlesCount = Count Int
                  | StartEnd
                    { start :: ZonedTime
                    , end :: ZonedTime
                    , includeFirst :: Bool
                    }
                  | StartCount
                    { start :: ZonedTime
                    , count :: Int
                    , includeFirst :: Bool
                    }
                  | EndCount
                    { end :: ZonedTime
                    , count :: Int
                    }
                  deriving (Show)

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday
               deriving (Show)

data Granularity = S5 | S10 | S15 | S30
                 | M1 | M2 | M3 | M4 | M5 | M10 | M15 | M30
                 | H1 | H2 | H3 | H4 | H6 | H8 | H12
                 | D
                 | W
                 | M
                 deriving (Show)

-- | Utility function to convert Granularity to NominalDiffTime. __NOTE__: The
-- conversion from month to NominalDiffTime is not correct in general; we just
-- assume 31 days in a month, which is obviously false for 5 months of the
-- year.
granularityToDiffTime :: Granularity -> NominalDiffTime
granularityToDiffTime S5 = fromSeconds' 5
granularityToDiffTime S10 = fromSeconds' 10
granularityToDiffTime S15 = fromSeconds' 15
granularityToDiffTime S30 = fromSeconds' 30
granularityToDiffTime M1 = fromSeconds' $ 1 * 60
granularityToDiffTime M2 = fromSeconds' $ 2 * 60
granularityToDiffTime M3 = fromSeconds' $ 3 * 60
granularityToDiffTime M4 = fromSeconds' $ 4 * 60
granularityToDiffTime M5 = fromSeconds' $ 5 * 60
granularityToDiffTime M10 = fromSeconds' $ 10 * 60
granularityToDiffTime M15 = fromSeconds' $ 15 * 60
granularityToDiffTime M30 = fromSeconds' $ 30 * 60
granularityToDiffTime H1 = fromSeconds' $ 1 * 60 * 60
granularityToDiffTime H2 = fromSeconds' $ 2 * 60 * 60
granularityToDiffTime H3 = fromSeconds' $ 3 * 60 * 60
granularityToDiffTime H4 = fromSeconds' $ 4 * 60 * 60
granularityToDiffTime H6 = fromSeconds' $ 6 * 60 * 60
granularityToDiffTime H8 = fromSeconds' $ 8 * 60 * 60
granularityToDiffTime H12 = fromSeconds' $ 12 * 60 * 60
granularityToDiffTime D = fromSeconds' $ 1 * 60 * 60 * 24
granularityToDiffTime W = fromSeconds' $ 7 * 60 * 60 * 24
granularityToDiffTime M = fromSeconds' $ 31 * 60 * 60 * 24

-- | Utility type for `midpointCandles` function response. Not exported.
data MidpointCandlesResponse = MidpointCandlesResponse
  { _midcandlesResponseInstrument  :: InstrumentText
  , _midcandlesResponseGranularity :: Text
  , _midcandlesResponseCandles     :: V.Vector MidpointCandlestick
  } deriving (Show, Generic)


instance FromJSON MidpointCandlesResponse where
  parseJSON = genericParseJSON $ jsonOpts "_midcandlesResponse"


-- | Utility type for `bidaskCandles` function response. Not exported.
data BidAskCandlesResponse = BidAskCandlesResponse
  { _bidaskResponseInstrument  :: InstrumentText
  , _bidaskResponseGranularity :: Text
  , _bidaskResponseCandles     :: V.Vector BidAskCandlestick
  } deriving (Show, Generic)


instance FromJSON BidAskCandlesResponse where
  parseJSON = genericParseJSON $ jsonOpts "_bidaskResponse"
