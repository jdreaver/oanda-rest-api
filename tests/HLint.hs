module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
  [ "--cpp-simple"
  , "src"
  , "tests"
  , "-i", "Use newtype instead of data"
  , "-i", "Use module export list"
  ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
