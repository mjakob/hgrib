{- |
Module      : Data.Grib.Raw.ValueSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Value.
-}

module Data.Grib.Raw.ValueSpec (main, spec) where

import Foreign

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gribGetLong" $ do
    it "should return 16 for numberOfPointsAlongAParallel" $
      withRegular1 $ \h ->
        gribGetLong h "Ni" `shouldReturn` 16

    it "should return 31 for numberOfPointsAlongAMeridian" $
      withRegular1 $ \h ->
        gribGetLong h "Nj" `shouldReturn` 31

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetLong h "missingKey" `shouldThrow` isGribException GribNotFound

  describe "gribGetDouble" $ do
    it "should return 60 for latitudeOfFirstGridPointInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "latitudeOfFirstGridPointInDegrees" `shouldReturn` 60

    it "should return 0 for longitudeOfFirstGridPointInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "longitudeOfFirstGridPointInDegrees" `shouldReturn` 0

    it "should return 0 for latitudeOfLastGridPointInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "latitudeOfLastGridPointInDegrees" `shouldReturn` 0

    it "should return 30 for longitudeOfLastGridPointInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "longitudeOfLastGridPointInDegrees" `shouldReturn` 30

    it "should return 2 for jDirectionIncrementInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "jDirectionIncrementInDegrees" `shouldReturn` 2

    it "should return 2 for iDirectionIncrementInDegrees" $
      withRegular1 $ \h ->
        gribGetDouble h "iDirectionIncrementInDegrees" `shouldReturn` 2

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetDouble h "missingKey" `shouldThrow` isGribException GribNotFound

  describe "gribGetLongArray" $ do
    it "should return [16] for numberOfPointsAlongAParallel" $
      withRegular1 $ \h -> allocaArray 10 $ \ls ->
        gribGetLongArray h "Ni" ls 10 `shouldReturn` [16]

    it "should return [31] for numberOfPointsAlongAMeridian" $
      withRegular1 $ \h -> allocaArray 10 $ \ls ->
        gribGetLongArray h "Nj" ls 10 `shouldReturn` [31]

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h -> allocaArray 10 $ \ls ->
        gribGetLongArray h "missingKey" ls 10
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribArrayTooSmall if the array is too small" $
      withRegular1 $ \h ->
        gribGetLongArray h "Ni" nullPtr 0
          `shouldThrow` isGribException GribArrayTooSmall

  describe "gribGetDoubleArray" $ do
    it "should return 496 values with an average of 291.585" $
      withRegular1 $ \h -> allocaArray 500 $ \ds -> do
        ds' <- gribGetDoubleArray h "values" ds 500
        length ds' `shouldBe` 496
        sum ds' / 496 `shouldBe` 291.5852483933972

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h -> allocaArray 10 $ \ds ->
        gribGetDoubleArray h "missingKey" ds 10
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribArrayTooSmall if the array is too small" $
      withRegular1 $ \h ->
        gribGetDoubleArray h "values" nullPtr 0
          `shouldThrow` isGribException GribArrayTooSmall

  describe "gribGetDoubleElement" $ do
    it "should return 279 for the 0th value" $
      withRegular1 $ \h ->
        gribGetDoubleElement h "values" 0 `shouldReturn` 279

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetDoubleElement h "missingKey" 0
          `shouldThrow` isGribException GribNotFound

  describe "gribGetDoubleElements" $ do
    it "should return [279] for the 0th value" $
      withRegular1 $ \h ->
        gribGetDoubleElements h "values" [0] `shouldReturn` [279]

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetDoubleElements h "missingKey" [0]
          `shouldThrow` isGribException GribNotFound

  describe "gribGetString" $ do
    it "should return \"grid_simple\" for packingType" $
      withRegular1 $ \h -> allocaBytes 12 $ \bufr ->
        gribGetString h "packingType" bufr 12 `shouldReturn` "grid_simple"

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h -> allocaBytes 2 $ \bufr ->
        gribGetString h "missingKey" bufr 2
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribBufferTooSmall if the buffer is too small" $
      withRegular1 $ \h -> allocaBytes 1 $ \bufr ->
        gribGetString h "packingType" bufr 1
          `shouldThrow` isGribException GribBufferTooSmall

  describe "gribGetBytes" $ do
    it "should return 2 for numberOfPointsAlongAParallel" $
      withRegular1 $ \h -> allocaArray 4 $ \bs ->
        gribGetBytes h "Ni" bs 4 >>= \(_, n) -> n `shouldBe` 2

    it "should return 2 for numberOfPointsAlongAMeridian" $
      withRegular1 $ \h -> allocaArray 4 $ \bs ->
        gribGetBytes h "Nj" bs 4 >>= \(_, n) -> n `shouldBe` 2

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h -> allocaArray 1 $ \bs ->
        gribGetBytes h "missingKey" bs 1
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribArrayTooSmall if the array is too small" $
      withRegular1 $ \h -> allocaArray 1 $ \bs ->
        gribGetBytes h "totalLength" bs 1
          `shouldThrow` isGribException GribArrayTooSmall

  describe "gribGetOffset" $ do
    it "should return 66 for numberOfPointsAlongAParallel" $
      withRegular1 $ \h ->
        gribGetOffset h "Ni" `shouldReturn` 66

    it "should return 68 for numberOfPointsAlongAMeridian" $
      withRegular1 $ \h ->
        gribGetOffset h "Nj" `shouldReturn` 68

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetOffset h "missingKey" `shouldThrow` isGribException GribNotFound

  describe "gribGetSize" $ do
    it "should return 496 for the values" $
      withRegular1 $ \h ->
        gribGetSize h "values" `shouldReturn` 496

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetSize h "missingKey" `shouldThrow` isGribException GribNotFound

  describe "gribGetLength" $ do
    it "should return 256 for packingType" $
      withRegular1 $ \h ->
        gribGetLength h "packingType" `shouldReturn` 256

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribGetLength h "missingKey" `shouldThrow` isGribException GribNotFound

  describe "gribSetLong" $ do
    it "should update centre to 80" $
      withRegular1 $ \h -> let newCentre = 80 in do
        gribSetLong h "centre" newCentre
        gribGetLong h "centre" `shouldReturn` newCentre

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribSetLong h "missingKey" 0 `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      withRegular1 $ \h ->
        gribSetLong h "getNumberOfValues" 0
          `shouldThrow` isGribException GribReadOnly

  describe "gribSetDouble" $ do
    it "should update missingValue to 0" $
      withRegular1 $ \h -> let newMissingValue = 0 in do
        gribSetDouble h "missingValue" newMissingValue
        gribGetDouble h "missingValue" `shouldReturn` newMissingValue
  
    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribSetDouble h "missingKey" 0 `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      withRegular1 $ \h ->
        gribSetDouble h "referenceValue" 0
          `shouldThrow` isGribException GribReadOnly

  describe "gribSetLongArray" $ do
    it "should update centre to 80" $
      withRegular1 $ \h -> let newCentre = 80 in do
        gribSetLongArray h "centre" [newCentre]
        gribGetLong h "centre" `shouldReturn` newCentre

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribSetLongArray h "missingKey" [1]
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      withRegular1 $ \h ->
        gribSetLongArray h "getNumberOfValues" [1]
          `shouldThrow` isGribException GribReadOnly

  describe "gribSetDoubleArray" $ do
    it "should set all values to zero" $
      withRegular1 $ \h -> gribGetSize h "values" >>= \n ->
      allocaArray n $ \ds -> let newVals = replicate n 0 in do
        gribSetDoubleArray h "values" newVals
        gribGetDoubleArray h "values" ds n `shouldReturn` newVals

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribSetDoubleArray h "missingKey" [1]
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      withRegular1 $ \h ->
        gribSetDoubleArray h "referenceValue" [1]
          `shouldThrow` isGribException GribReadOnly

  describe "gribSetString" $ do
    it "should set a new file key and return its length" $
      withRegular1 $ \h ->
      let len = length regular1Path in
      allocaBytes (len + 1) $ \cs -> do
        gribSetString h "file" regular1Path `shouldReturn` len
        gribGetString h "file" cs (len + 1) `shouldReturn` regular1Path

    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h ->
        gribSetString h "missingKey" ""
          `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      withRegular1 $ \h ->
        gribSetString h "referenceValue" ""
          `shouldThrow` isGribException GribReadOnly

  describe "gribSetBytes" $
    it "should fail with GribNotFound if the key is missing" $
      withRegular1 $ \h -> allocaArray 1 $ \bs ->
        gribSetBytes h "missingKey" bs 1
          `shouldThrow` isGribException GribNotFound

  describe "gribCopyNamespace" $
    it "should succeed to copy namespace ls to itself" $
      withRegular1 $ \h ->
        gribCopyNamespace h (Just "ls") h
