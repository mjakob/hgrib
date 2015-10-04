{- |
Module      : Data.Grib.Raw.IndexSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Index.
-}

module Data.Grib.Raw.IndexSpec ( main, spec ) where

import Control.Monad ( void )
import Foreign       ( alloca )

import Test.Hspec
import Data.Grib.Exception
import Data.Grib.Raw
import Data.Grib.Raw.Test hiding (withRegular1)


main :: IO ()
main = hspec spec

withGribIndex :: FilePath -> [Key] -> (GribIndex -> IO ()) -> IO ()
withGribIndex name keys = (gribIndexNewFromFile defaultGribContext name keys >>=)

withRegular1 :: [Key] -> (GribIndex -> IO ()) -> IO ()
withRegular1 = withGribIndex regular1Path

isGribEndOfIndex :: GribException -> Bool
isGribEndOfIndex = isGribException GribEndOfIndex

spec :: Spec
spec = do
  let ctx  = defaultGribContext
      keys = ["paramId", "number"]

  describe "gribIndexGetSize" $
    it "should return 1 for Ni" $
      withRegular1 ["Ni"] $ \idx ->
        gribIndexGetSize idx "Ni" `shouldReturn` 1

  describe "gribIndexGetLong" $
    it "should return [16] for Ni" $
      withRegular1 ["Ni"] $ \idx -> alloca $ \l ->
        gribIndexGetLong idx "Ni" l 1 `shouldReturn` [16]

  describe "gribIndexGetDouble" $
    it "should return [60] for yFirst" $
      withRegular1 ["yFirst"] $ \idx -> alloca $ \d ->
        gribIndexGetDouble idx "yFirst" d 1 `shouldReturn` [60]

  describe "gribIndexGetString" $
    it "should return [\"grid_simple\"] for packingType" $
      withRegular1 ["packingType"] $ \idx -> alloca $ \s ->
        gribIndexGetString idx "packingType" s 1 `shouldReturn` ["grid_simple"]

  describe "gribHandleNewFromIndex" $ do
      it "should succeed after gribIndexSelectLong \"Ni\" 16" $
        withRegular1 ["Ni"] $ \idx -> do
          gribIndexSelectLong idx "Ni" 16 `shouldReturn` ()
          void $ gribHandleNewFromIndex idx

      it "should succeed after gribIndexSelectDouble \"yFirst\" 60" $
        withRegular1 ["yFirst"] $ \idx -> do
          gribIndexSelectDouble idx "yFirst" 60 `shouldReturn` ()
          void $ gribHandleNewFromIndex idx

      it "should succeed after gribIndexSelectString \"packingtype\" \
         \\"grid_simple\"" $
        withRegular1 ["packingType"] $ \idx -> do
          gribIndexSelectString idx "packingType" "grid_simple" `shouldReturn` ()
          void $ gribHandleNewFromIndex idx

      it "should fail with GribEndOfIndex after selecting nothing" $
        withRegular1 ["Ni"] $ \idx -> do
          gribIndexSelectLong idx "Ni" 0 `shouldReturn` ()
          void $ gribHandleNewFromIndex idx `shouldThrow` isGribEndOfIndex

  describe "gribIndexNew and gribIndexAddFile" $
    it "should create a new index and add a file to it" $ do
      idx <- gribIndexNew ctx keys
      gribIndexAddFile idx regular1Path `shouldReturn` ()
      gribIndexGetSize idx "paramId" `shouldReturn` 1

  describe "gribIndexWrite and gribIndexRead" $
    let idxPath = "test/stage/test.grib.idx" in
    after_ (safeRemoveFile idxPath) $
      it "should write an index to file and read it back in again" $
        withRegular1 keys $ \idx -> do
          gribIndexWrite idx idxPath `shouldReturn` ()
          idx' <- gribIndexRead ctx idxPath
          gribIndexGetSize idx' "paramId" `shouldReturn` 1
