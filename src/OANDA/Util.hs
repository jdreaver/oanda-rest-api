{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions.

module OANDA.Util
       ( constructOpts
       , makeParams
       , commaList
       , jsonOpts
       , jsonResponse
       , jsonResponseArray
       ) where

import           Data.Aeson (FromJSON)
import qualified Data.Aeson.TH as TH
import           Control.Lens
import           Data.ByteString (append)
import           Data.Char (toLower)
import qualified Data.Map as Map
import           Data.Text (Text, intercalate)
import           Network.Wreq

import           OANDA.Types


-- | Create options for Wreq `getWith` using access token and params.
constructOpts :: AccessToken -> [(Text, [Text])] -> Options
constructOpts (AccessToken t) ps = defaults & params' & header'
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
