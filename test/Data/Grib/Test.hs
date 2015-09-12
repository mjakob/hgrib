{- |
Module      : Data.Grib.Test
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Utilities for the unit tests of HGrib's high-level interface.
-}

module Data.Grib.Test
       ( gShouldBe
       , gShouldReturn
       , notAGribPath
       , regular1Path
       , regular2Path
       , testUuidPath
       ) where

import Test.Hspec ( shouldBe )

import Data.Grib


type GExpectation = GribIO ()

gShouldBe :: (Show a, Eq a) => a -> a -> GExpectation
x `gShouldBe` y = liftIO $ x `shouldBe` y

gShouldReturn :: (Show a, Eq a) => GribIO a -> a -> GExpectation
m `gShouldReturn` r = m >>= (`gShouldBe` r)

notAGribPath :: FilePath
notAGribPath = "test/stage/not_a_grib.txt"

regular1Path :: FilePath
regular1Path = "test/stage/regular_latlon_surface.grib1"

regular2Path :: FilePath
regular2Path = "test/stage/regular_latlon_surface.grib2"

testUuidPath :: FilePath
testUuidPath = "test/stage/test_uuid.grib2"
