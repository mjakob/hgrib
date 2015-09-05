{- |
Module      : Data.Grib.Raw.ContextSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Context.
-}

module Data.Grib.Raw.ContextSpec ( main, spec ) where

import Control.Exception ( bracket )

import Test.Hspec
import Data.Grib.Raw


main :: IO ()
main = hspec spec

withGribexMode :: GribContext -> IO () -> IO ()
withGribexMode ctx action =
  bracket saveMode restoreMode (const action)
  where saveMode          = gribGetGribexMode ctx
        restoreMode True  = gribGribexModeOn ctx
        restoreMode False = gribGribexModeOff ctx

spec :: Spec
spec = do
  let ctx = defaultGribContext
  
  describe "gribContextGetDefault" $
    it "should not return a null context" $
      gribContextGetDefault `shouldNotReturn` defaultGribContext

  describe "gribContextNew" $ do
    it "should not return a null context" $
      gribContextNew ctx `shouldNotReturn` defaultGribContext

    it "should not return the default context" $
      gribContextGetDefault >>= (gribContextNew ctx `shouldNotReturn`)

  describe "gribGtsHeaderOff" $
    it "should return nothing" $
      gribGtsHeaderOff ctx `shouldReturn` ()

  describe "gribGtsHeaderOn" $ after_ (gribGtsHeaderOff ctx) $
    it "should return nothing" $
      gribGtsHeaderOn ctx `shouldReturn` ()

  describe "gribGribexModeOn" $ around_ (withGribexMode ctx) $
    it "should enable gribex mode" $ do
      gribGribexModeOn ctx `shouldReturn` ()
      gribGetGribexMode ctx `shouldReturn` True

  describe "gribGribexModeOff" $ around_ (withGribexMode ctx) $
      it "should disable gribex mode" $ do
        gribGribexModeOff ctx `shouldReturn` ()
        gribGetGribexMode ctx `shouldReturn` False

  describe "gribMultiSupportOff" $
    it "should return nothing" $
      gribMultiSupportOff ctx `shouldReturn` ()

  describe "gribMultiSupportOn" $ after_ (gribMultiSupportOff ctx) $
    it "should return nothing" $
      gribMultiSupportOn ctx `shouldReturn` ()
