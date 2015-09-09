{- |
Module      : Data.Grib.Raw.HandleSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.Handle.
-}

module Data.Grib.Raw.HandleSpec ( main, spec ) where

import Control.Monad ( void )
import Foreign       ( allocaBytes, nullPtr )

import Test.Hspec
import Data.Grib.Raw
import Data.Grib.Raw.Test


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let ctx = defaultGribContext
  
  describe "gribHandleNewFromSamples" $ do
    it "should succeed for the GRIB1 resource" $
      void $ gribHandleNewFromSamples ctx "GRIB1"

    it "should fail with NullPtrReturned for a missing resource" $
      gribHandleNewFromSamples ctx "GRIB0" `shouldThrow` isNullPtrReturned

  describe "gribHandleClone" $
    it "should succeed" $
      withRegular1 $ \h ->
        void $ gribHandleClone h

  describe "gribWriteMessage" $
    let gribPath = "test/stage/test.grib" in
    after_ (safeRemoveFile gribPath) $
      it "should write a message to file" $
        withRegular1 $ \h -> do
          gribWriteMessage h gribPath "wb" `shouldReturn` ()
          withBinaryCFile gribPath ReadMode $ \f ->
            void $ gribHandleNewFromFile ctx f

  describe "gribGetMessage" $
    it "should return a message of length 1100" $
      withRegular1 $ \h -> do
        (_, len) <- gribGetMessage h
        len `shouldBe` 1100

  describe "gribGetMessageCopy" $ do
    it "should return a message of length 1100" $
      withRegular1 $ \h -> allocaBytes 1101 $ \p -> do
        (_, len) <- gribGetMessageCopy h p 1101
        len `shouldBe` 1100

    it "should fail with GribBufferTooSmall when the allocation is too small" $
      withRegular1 $ \h ->
        gribGetMessageCopy h nullPtr 0
          `shouldThrow` isGribException GribBufferTooSmall

  describe "gribHandleNewFromMessage" $
    it "should succeed if the message is valid" $
      withRegular1 $ \h ->
        void $ gribGetMessage h >>= uncurry (gribHandleNewFromMessage ctx)

  describe "gribHandleNewFromMessageCopy" $ do
    it "should succeed if the message is valid" $
      withRegular1 $ \h ->
        void $ gribGetMessage h >>= uncurry (gribHandleNewFromMessageCopy ctx)

    it "should fail with NullPtrReturned if the message is invalid" $
      gribHandleNewFromMessageCopy ctx nullPtr 0
        `shouldThrow` isNullPtrReturned

  describe "gribHandleNewFromMultiMessage" $
    let checkMultiMessageLength = withRegular1 $ \h -> do
          (msg, len) <- gribGetMessage h
          (_, _, len') <- gribHandleNewFromMultiMessage ctx msg len
          len' `shouldBe` 0
    in do
      context "in multi mode" $ after_ (gribMultiSupportOff ctx) $
        it "should return a message of zero length if given one message" $ do
           gribMultiSupportOn ctx
           checkMultiMessageLength

      context "not in multi mode" $
        it "should return a message of zero length if given one message"
           checkMultiMessageLength

  describe "gribMultiHandleAppend" $
    it "should succeed if the handle is valid" $
      withRegular1 $ \h ->
        void $ gribMultiHandleNew ctx >>= gribMultiHandleAppend h 4

  describe "gribMultiHandleWrite" $
    let gribPath = "test/stage/test.grib" in
    after_ (safeRemoveFile gribPath) $
    after_ (gribMultiSupportOff ctx) $
      it "should write a multi message to file" $ do
        gribMultiSupportOn ctx
        withRegular2 $ \h -> do
          mh <- gribMultiHandleNew ctx
          gribMultiHandleAppend h 4 mh
          gribMultiHandleAppend h 4 mh
          withBinaryCFile gribPath WriteMode $ \f ->
            gribMultiHandleWrite mh f
          withGribFile gribPath $ \h' ->
            gribGetLong h' "editionNumber" `shouldReturn` 2

  describe "gribCountInFile" $
    it "should return 1 for the regular grib file" $
      withBinaryCFile regular1Path ReadMode $ \f ->
        gribCountInFile ctx f `shouldReturn` 1
