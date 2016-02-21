-- Run doctests for HGrib.

module Main ( main ) where

import Test.DocTest ( doctest )

main :: IO ()
main = doctest ["-idist/build", "-isrc", "-lgrib_api", "src"]
