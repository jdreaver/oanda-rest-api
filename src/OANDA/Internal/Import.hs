-- | Common imports used throughout the library. Easier to just put them in one
-- spot.

module OANDA.Internal.Import
  ( module X
  , (.~)
  , (^.)
  , (&)
  , mzero
  , toLower
  , catMaybes
  , Text
  , intercalate
  , unpack
  , pack
  , Generic
  , defaultTimeLocale
  , (<>)
  ) where

import Control.Lens ((.~), (^.), (&))
import Control.Monad (mzero)
import Data.Aeson as X
import Data.Char (toLower)
import Data.Decimal as X
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Scientific as X
import Data.Text (Text, intercalate, unpack, pack)
import Data.Text.Encoding as X
import Data.Thyme as X
import Data.Thyme.Format.Aeson as X ()
import GHC.Generics (Generic)
import Network.HTTP.Simple as X
import System.Locale (defaultTimeLocale)
