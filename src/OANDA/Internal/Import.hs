-- | Common imports used throughout the library. Easier to just put them in one
-- spot.

module OANDA.Internal.Import
  ( module X
  , unPrefix
  ) where

import Control.Lens as X
  ( (.~)
  , (^.)
  , (&)
  , makeLenses
  )
import Control.Monad as X (mzero)
import Data.Aeson as X
import Data.Aeson.TH as X
import Data.Aeson.Types as X
import Data.Char as X (toLower)
import Data.Decimal as X
import Data.Maybe as X (catMaybes)
import Data.Monoid as X ((<>))
import Data.Scientific as X
import Data.String as X (IsString (..), fromString)
import Data.Text as X (Text, unpack, pack)
import Data.Text.Encoding as X
import Data.Thyme as X
import Data.Thyme.Format.Aeson as X ()
import Network.HTTP.Simple as X
import System.Locale as X (defaultTimeLocale)

-- | Aeson Options that remove the prefix from fields
unPrefix :: String -> Options
unPrefix prefix = defaultOptions
  { fieldLabelModifier = unCapitalize . dropPrefix prefix
  , omitNothingFields = True
  }

-- | Lower case leading character
unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

-- | Remove given prefix
dropPrefix :: String -> String -> String
dropPrefix prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " <> pre
    go [] (c:cs) = c : cs
    go (p:preRest) (c:cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " <>  (p:preRest)  <> " " <> (c:cRest)

    contextual msg = "dropPrefix: " <> msg <> ". " <> prefix <> " " <> input
