{- |
Module      : Data.Grib.Raw.KeysIteratorSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.KeysIterator.
-}

module Data.Grib.Raw.KeysIteratorSpec (main, spec) where

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "GribKeysIterator" $ do
    it "should return parametersVersion in the 1st step with no filters" $
      withRegular1 $ \h -> withGribKeysIterator h [] Nothing $ \kiter -> do
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "parametersVersion"

    it "should return totalLength in the 1st step with SkipComputed" $
      let flags = [GribKeysIteratorSkipComputed] in
      withRegular1 $ \h -> withGribKeysIterator h flags Nothing $ \kiter -> do
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "totalLength"

    it "should return UseEcmfConventions in the 2nd step with SkipComputed" $
      let flags = [GribKeysIteratorSkipComputed] in
      withRegular1 $ \h -> withGribKeysIterator h flags Nothing $ \kiter -> do
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "editionNumber"

    skipIfGribApiVersion (< 11300) $  -- GRIB API issue GRIB-566
      it "should return totalLength when SkipComputed is set after creation" $
      let flags = [GribKeysIteratorSkipComputed] in
      withRegular1 $ \h -> withGribKeysIterator h [] Nothing $ \kiter -> do
        gribKeysIteratorSetFlags kiter flags
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "totalLength"

    it "should return edition in the 1st step in namespace ls" $
      withRegular1 $ \h -> withGribKeysIterator h [] (Just "ls") $ \kiter -> do
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "edition"

    it "should return parametersVersion after rewinding with no filters" $
      withRegular1 $ \h -> withGribKeysIterator h [] Nothing $ \kiter -> do
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorRewind kiter
        gribKeysIteratorNext kiter `shouldReturn` True
        gribKeysIteratorGetName kiter `shouldReturn` "parametersVersion"
