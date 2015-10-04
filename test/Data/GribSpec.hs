{- |
Module      : Data.GribSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.
-}

module Data.GribSpec ( main, spec ) where

import Control.Exception ( throwIO )
import System.IO.Error   ( isDoesNotExistError )

import Test.Hspec
import Data.Grib
import Data.Grib.Exception
import Data.Grib.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runGribIO" $ do
    it "should return the results of the actions" $
      runGribIO testUuidPath (getLong "topLevel") `shouldReturn` [1, 2]

    it "should fail with DoesNotExistError for a non-existing file" $
      runGribIO "doesnotexist" (return ()) `shouldThrow` isDoesNotExistError

    it "should fail with a GribException for a non-grib file" $
      runGribIO notAGribPath (return ()) `shouldThrow` isAnyGribException

    it "should fail with any exception raised by the action" $
      runGribIO testUuidPath (liftIO $ throwIO NullPtrReturned)
        `shouldThrow` isNullPtrReturned

  describe "runGribIO_" $ do
    it "should return unit" $
      runGribIO_ testUuidPath (getLong "topLevel") `shouldReturn` ()

    it "should fail with DoesNotExistError for a non-existing file" $
      runGribIO_ "doesnotexist" (return ()) `shouldThrow` isDoesNotExistError

    it "should fail with a GribException for a non-grib file" $
      runGribIO_ notAGribPath (return ()) `shouldThrow` isAnyGribException

    it "should fail with any exception raised by the action" $
      runGribIO_ testUuidPath (liftIO $ throwIO NullPtrReturned)
        `shouldThrow` isNullPtrReturned

  describe "getDouble" $
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (getDouble "missingKey")
        `shouldThrow` isGribException GribNotFound

  describe "getLong" $
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (getLong "missingKey")
        `shouldThrow` isGribException GribNotFound

  describe "getString" $
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (getString "missingKey")
        `shouldThrow` isGribException GribNotFound

  describe "setDouble" $ do
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (setDouble "missingKey" 0)
        `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      runGribIO_ testUuidPath (setDouble "referenceValue" 0)
        `shouldThrow` isGribException GribReadOnly

  describe "setLong" $ do
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (setLong "missingKey" 0)
        `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      runGribIO_ testUuidPath (setLong "getNumberOfValues" 0)
        `shouldThrow` isGribException GribReadOnly

  describe "setString" $ do
    it "should fail with GribNotFound if the key does not exist" $
      runGribIO_ testUuidPath (setString "missingKey" "value")
        `shouldThrow` isGribException GribNotFound

    it "should fail with GribReadOnly if the key is read-only" $
      runGribIO_ testUuidPath (setString "referenceValue" "value")
        `shouldThrow` isGribException GribReadOnly

  describe "getFilename" $
    it "should return the name of the file being read" $
      runGribIO testUuidPath getFilename `shouldReturn` replicate 2 testUuidPath

  describe "getIndex" $
    it "should return the zero-based index of the current message in the file" $
      runGribIO testUuidPath getIndex `shouldReturn` [0, 1]

  describe "get example" $
    it "should produce the same result as the original" $
      runGribIO_ regular1Path $ do
        setString "file" regular1Path

        getLong "Ni" `gShouldReturn` 16
        getLong "Nj" `gShouldReturn` 31

        getDouble "yFirst"      `gShouldReturn` 60
        getDouble "xFirst"      `gShouldReturn`  0
        getDouble "yLast"       `gShouldReturn`  0
        getDouble "xLast"       `gShouldReturn` 30
        getDouble "DyInDegrees" `gShouldReturn`  2
        getDouble "DxInDegrees" `gShouldReturn`  2

        getString "packingType" `gShouldReturn` "grid_simple"

        values <- getValues
        let nvalues = length values
            avg     = sum values / fromIntegral nvalues
        nvalues `gShouldBe` 496
        avg     `gShouldBe` 291.5852483933972

        getString "file" `gShouldReturn` regular1Path
