{- |
Module      : Data.Grib.Raw.FieldsetSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Fieldset.
-}

module Data.Grib.Raw.FieldsetSpec ( main, spec ) where

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

withRegular12 :: (GribFieldset -> IO ()) -> IO ()
withRegular12 = (gribFieldsetNewFromFiles ctx filenames keys Nothing Nothing >>=)
  where ctx       = defaultGribContext
        filenames = [regular1Path, regular2Path]
        keys      = ["step", "date", "param", "levelType"]

spec :: Spec
spec = do
  describe "gribFieldsetApplyOrderBy" $
    it "should succeed for order by step" $
      withRegular12 $ \set ->
        gribFieldsetApplyOrderBy set "step"

  describe "gribFieldsetCount" $
    it "should return 2 files" $
      withRegular12 $ \set -> do
        gribFieldsetApplyOrderBy set "step"
        gribFieldsetCount set `shouldBe` 2
