{- |
Module      : Data.Grib.Raw.NearestSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Nearest.
-}

module Data.Grib.Raw.NearestSpec (main, spec) where

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gribNearestFind" $
    it "should return indices [248, 247, 232, 231] for a point (31, 15)" $
      withRegular1 $ \h -> withGribNearest h $ \nearest ->
        gribNearestFind nearest h 31 15 [] `shouldReturn`
          ([30, 30, 32, 32], [16, 14, 16, 14],
           [289.1650390625, 288.1396484375, 289.1982421875, 288.2177734375],
           [146.6943567638852 , 146.6943567638852 ,
            146.04418798713152, 146.04418798713152],
           [248, 247, 232, 231])

  describe "gribNearestFindMultiple" $
    it "should return indices [247, 248] for points (31, 14) and (31, 16)" $
      withRegular1 $ \h ->
        gribNearestFindMultiple h False [31, 31] [14, 16] `shouldReturn`
          ([30, 30], [14, 16], [288.1396484375, 289.1650390625],
           [111.13331652195225, 111.13331652195225], [247, 248])
