-- Run hlint on hgrib's source code.

module Main ( main ) where

import Language.Haskell.HLint ( hlint )
import System.Exit            ( exitFailure, exitSuccess )

main :: IO ()
main = do
  hints <- hlint ["src", "test"]
  if null hints then exitSuccess else exitFailure
