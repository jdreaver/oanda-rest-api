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
       , Candlestick (..)
       , candles
       , CandlesArgs (..)
       , candlesArgs
       , CandlesCount (..)
       , CandleFormat (..)
       , DayOfWeek (..)
       , Granularity (..)
       ) where

import           Data.Aeson
import           Data.Char (toLower)
import           Data.Text (Text, pack)
import           Data.Time
import qualified Data.Vector as V
import           GHC.Generics (Generic)

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


-- | Retrieve the price history of a single instrument
candles :: OandaData -> InstrumentText -> CandlesArgs -> IO (V.Vector Candlestick)
candles od i (CandlesArgs c g cfmt di atz wa) =
  do let url   = baseURL od ++ "/v1/candles"
         cfmt' = map toLower (show cfmt)
         opts  = constructOpts od $ [ ("instrument", [pack i])
                                    , ("granularity", [pack $ show g])
                                    , ("candleFormat", [pack cfmt'])
                                    , ("dailyAlignment", [pack $ show di])
                                    , ("alignmentTimeZone", [pack atz])
                                    , ("weeklyAlignment", [pack $ show wa])
                                    ] ++ countOpts c

     response <- jsonResponse url opts :: IO CandlesResponse
     return $ _candlesResponseCandles response
  where countOpts (Count count) = [("count", [pack $ show count])]
        countOpts (StartEnd st ed incf) = [ ("start", [pack $ formatTimeRFC3339 st])
                                          , ("end", [pack $ formatTimeRFC3339 ed])
                                          , ("includeFirst", [pack $ map toLower (show incf)])
                                          ]

data Candlestick = Candlestick
  { candlesticktime     :: ZonedTime
  , candlestickopenMid  :: Double
  , candlestickhighMid  :: Double
  , candlesticklowMid   :: Double
  , candlestickcloseMid :: Double
  , candlestickvolume   :: Int
  , candlestickcomplete :: Bool
  } deriving (Show, Generic)

instance FromJSON Candlestick where
  parseJSON = genericParseJSON $ jsonOpts "candlestick"


data CandlesArgs = CandlesArgs
  { candlesCount           :: CandlesCount
  , candlesGranularity     :: Granularity
  , candlesCandleFormat    :: CandleFormat
  , candlesDailyAlignment  :: Int
  , candlesAlignmentTZ     :: String
  , candlesWeeklyAlignment :: DayOfWeek
  } deriving (Show)

candlesArgs :: CandlesArgs
candlesArgs = CandlesArgs (Count 500) S5 Midpoint 17 "America/New_York" Friday

data CandlesCount = Count Int
                  | StartEnd { start :: ZonedTime
                             , end   :: ZonedTime
                             , includeFirst :: Bool
                             }
                  deriving (Show)

data CandleFormat = Midpoint
                  | BidAsk
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

-- | Utility type for `candles` function response. Not exported.
data CandlesResponse = CandlesResponse
  { _candlesResponseInstrument :: InstrumentText
  , _candlesResponseGranularity :: String
  , _candlesResponseCandles :: V.Vector Candlestick
  } deriving (Show, Generic)


instance FromJSON CandlesResponse where
  parseJSON = genericParseJSON $ jsonOpts "_candlesResponse"
