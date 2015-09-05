{- |
Module      : Data.Grib.Raw.IteratorSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Iterator.
-}

module Data.Grib.Raw.IteratorSpec ( main, spec ) where

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "GribIterator" $ do
    it "should have next after creation" $
      withRegular1 $ \h -> withGribIterator h 0 $ \iter ->
        gribIteratorHasNext iter `shouldReturn` True

    it "should not have previous after creation" $
      withRegular1 $ \h -> withGribIterator h 0 $ \iter -> do
        (status, _, _, _) <- gribIteratorPrevious iter
        status `shouldBe` False
    
    it "should return (True, 60, 0, 279) in the 1st step" $
      withRegular1 $ \h -> withGribIterator h 0 $ \iter ->
        gribIteratorNext iter `shouldReturn` (True, 60, 0, 279)

    it "should return (True, 60, 2, 279.961) in the 2nd step" $
      withRegular1 $ \h -> withGribIterator h 0 $ \iter -> do
        _ <- gribIteratorNext iter
        gribIteratorNext iter `shouldReturn` (True, 60, 2, 279.9609375)

    it "should return (True, 60, 0, 279) after reset" $
      withRegular1 $ \h -> withGribIterator h 0 $ \iter -> do
        _ <- gribIteratorNext iter
        gribIteratorReset iter
        gribIteratorNext iter `shouldReturn` (True, 60, 0, 279)
