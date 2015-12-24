{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions.

module OANDA.Util
       ( constructOpts
       , makeParams
       , commaList
       , jsonOpts
       ) where

import qualified Data.Aeson.TH as TH
import           Control.Lens
import           Data.Char (toLower)
import           Data.ByteString (append)
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
