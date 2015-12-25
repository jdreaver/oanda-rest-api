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
       ) where

import           Data.Aeson
import           Data.Char (toLower)
import           Data.Text (Text, pack)
import           Data.Time
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           Network.Wreq (Options)

import           OANDA.Util
import           OANDA.Types


data InstrumentsArgs = InstrumentsArgs
  { instrumentsFields      :: [Text]
  , instrumentsInstruments :: [Text]
  } deriving (Show)

instrumentsArgs :: InstrumentsArgs
instrumentsArgs = InstrumentsArgs ["displayName", "pip", "maxTradeUnits"] []

data Instrument = Instrument
  { instrumentInstrument    :: String
  , instrumentPip           :: Maybe String
  , instrumentMaxTradeUnits :: Maybe Double
  , instrumentDisplayName   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Instrument where
  parseJSON = genericParseJSON $ jsonOpts "instrument"

-- | Retrieve a list of instruments from OANDA
instruments :: OandaData -> AccountID -> InstrumentsArgs -> IO (V.Vector Instrument)
instruments od (AccountID aid) (InstrumentsArgs fs is) = do
  let url = baseURL od ++ "/v1/instruments"
      opts = constructOpts od [ ("accountId", [pack $ show aid])
                              , ("instruments", is)
                              , ("fields", fs)
                              ]
  jsonResponseArray url opts "instruments"


-- | Retrieve the current prices for a list of instruments.
prices :: OandaData -> [InstrumentText] -> Maybe ZonedTime -> IO (V.Vector Price)
prices od is zt =
  do let url = baseURL od ++ "/v1/prices"
         ztOpt = maybe [] (\zt' -> [("since", [pack $ formatTimeRFC3339 zt'])]) zt
         opts = constructOpts od $ ("instruments", map pack is) : ztOpt

     jsonResponseArray url opts "prices"

data Price = Price
  { priceInstrument :: InstrumentText
  , priceTime       :: ZonedTime
  , priceBid        :: Double
  , priceAsk        :: Double
  } deriving (Show, Generic)

instance FromJSON Price where
  parseJSON = genericParseJSON $ jsonOpts "price"


-- | Retrieve the price history of a single instrument in midpoint candles
midpointCandles :: OandaData -> InstrumentText -> CandlesArgs ->
                   IO (V.Vector MidpointCandlestick)
midpointCandles od i args =
  do let (url, opts) = candleOpts od i args "midpoint"
     response <- jsonResponse url opts :: IO MidpointCandlesResponse
     return $ _midcandlesResponseCandles response

-- | Retrieve the price history of a single instrument in bid/ask candles
bidaskCandles :: OandaData -> InstrumentText -> CandlesArgs ->
                 IO (V.Vector BidAskCandlestick)
bidaskCandles od i args =
  do let (url, opts) = candleOpts od i args "bidask"
     response <- jsonResponse url opts :: IO BidAskCandlesResponse
     return $ _bidaskResponseCandles response


-- | Utility function for both candle history functions
candleOpts :: OandaData -> InstrumentText -> CandlesArgs -> String -> (String, Options)
candleOpts od i (CandlesArgs c g di atz wa) fmt = (url, opts)
  where url   = baseURL od ++ "/v1/candles"
        opts  = constructOpts od $ [ ("instrument", [pack i])
                                   , ("granularity", [pack $ show g])
                                   , ("candleFormat", [pack fmt])
                                   , ("dailyAlignment", [pack $ show di])
                                   , ("alignmentTimeZone", [pack atz])
                                   , ("weeklyAlignment", [pack $ show wa])
                                   ] ++ countOpts c
        countOpts (Count count) = [("count", [pack $ show count])]
        countOpts (StartEnd st ed incf) = [ ("start", [pack $ formatTimeRFC3339 st])
                                          , ("end", [pack $ formatTimeRFC3339 ed])
                                          , ("includeFirst", [pack $ map toLower (show incf)])
                                          ]

data MidpointCandlestick = MidpointCandlestick
  { midpointCandlestickTime     :: ZonedTime
  , midpointCandlestickOpenMid  :: Double
  , midpointCandlestickHighMid  :: Double
  , midpointCandlestickLowMid   :: Double
  , midpointCandlestickCloseMid :: Double
  , midpointCandlestickVolume   :: Int
  , midpointCandlestickComplete :: Bool
  } deriving (Show, Generic)

instance FromJSON MidpointCandlestick where
  parseJSON = genericParseJSON $ jsonOpts "midpointCandlestick"


data BidAskCandlestick = BidAskCandlestick
  { bidaskCandlestickTime     :: ZonedTime
  , bidaskCandlestickOpenBid  :: Double
  , bidaskCandlestickOpenAsk  :: Double
  , bidaskCandlestickHighBid  :: Double
  , bidaskCandlestickHighAsk  :: Double
  , bidaskCandlestickLowBid   :: Double
  , bidaskCandlestickLowAsk   :: Double
  , bidaskCandlestickCloseBid :: Double
  , bidaskCandlestickCloseAsk :: Double
  , bidaskCandlestickVolume   :: Int
  , bidaskCandlestickComplete :: Bool
  } deriving (Show, Generic)

instance FromJSON BidAskCandlestick where
  parseJSON = genericParseJSON $ jsonOpts "bidaskCandlestick"

data CandlesArgs = CandlesArgs
  { candlesCount           :: CandlesCount
  , candlesGranularity     :: Granularity
  , candlesDailyAlignment  :: Int
  , candlesAlignmentTZ     :: String
  , candlesWeeklyAlignment :: DayOfWeek
  } deriving (Show)

candlesArgs :: CandlesArgs
candlesArgs = CandlesArgs (Count 500) S5 17 "America/New_York" Friday

data CandlesCount = Count Int
                  | StartEnd { start :: ZonedTime
                             , end   :: ZonedTime
                             , includeFirst :: Bool
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

-- | Utility type for `midpointCandles` function response. Not exported.
data MidpointCandlesResponse = MidpointCandlesResponse
  { _midcandlesResponseInstrument  :: InstrumentText
  , _midcandlesResponseGranularity :: String
  , _midcandlesResponseCandles     :: V.Vector MidpointCandlestick
  } deriving (Show, Generic)


instance FromJSON MidpointCandlesResponse where
  parseJSON = genericParseJSON $ jsonOpts "_midcandlesResponse"


-- | Utility type for `bidaskCandles` function response. Not exported.
data BidAskCandlesResponse = BidAskCandlesResponse
  { _bidaskResponseInstrument  :: InstrumentText
  , _bidaskResponseGranularity :: String
  , _bidaskResponseCandles     :: V.Vector BidAskCandlestick
  } deriving (Show, Generic)


instance FromJSON BidAskCandlesResponse where
  parseJSON = genericParseJSON $ jsonOpts "_bidaskResponse"
