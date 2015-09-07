{- |
Module      : Data.Grib.Raw.CFileSpec
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Unit and regression tests for Data.Grib.Raw.CFile.
-}

module Data.Grib.Raw.CFileSpec ( main, spec ) where

import Control.Exception ( bracket )
import Foreign           ( nullPtr )
import System.Directory  ( getTemporaryDirectory, removeFile )
import System.IO         ( hClose, openTempFile )
import System.IO.Error   ( isDoesNotExistError )

import Test.Hspec
import Data.Grib.Raw


main :: IO ()
main = hspec spec

createTempFile :: IO FilePath
createTempFile = do
  tempDir <- getTemporaryDirectory
  (path, h) <- openTempFile tempDir "CFile.test"
  hClose h
  return path

withTempFile :: (FilePath -> IO ()) -> IO ()
withTempFile = bracket createTempFile removeFile

spec :: Spec
spec = do
  describe "openBinaryCFile" $ do
    it "should open an existing file in ReadMode" $ withTempFile $ \name -> do
      f <- openBinaryCFile name ReadMode
      f `shouldNotBe` nullPtr
      closeCFile f

    it "should fail to open a non-existing file in ReadMode" $
      openBinaryCFile "I-dont-exist.test" ReadMode
        `shouldThrow` isDoesNotExistError

    it "should open a file in WriteMode" $ withTempFile $ \name -> do
      f <- openBinaryCFile name WriteMode
      f `shouldNotBe` nullPtr
      closeCFile f

    it "should open a file in AppendMode" $ withTempFile $ \name -> do
      f <- openBinaryCFile name AppendMode
      f `shouldNotBe` nullPtr
      closeCFile f

    it "should open a file in ReadWriteMode" $ withTempFile $ \name -> do
      f <- openBinaryCFile name ReadWriteMode
      f `shouldNotBe` nullPtr
      closeCFile f

  describe "withBinaryCFile" $ do
    it "should open an existing file in ReadMode" $ withTempFile $ \name ->
      withBinaryCFile name ReadMode (`shouldNotBe` nullPtr)

    it "should fail to open a non-existing file in ReadMode" $
      withBinaryCFile "I-dont-exist.test" ReadMode (\_ -> return ())
        `shouldThrow` isDoesNotExistError

    it "should open a file in WriteMode" $ withTempFile $ \name ->
      withBinaryCFile name WriteMode (`shouldNotBe` nullPtr)

    it "should open a file in AppendMode" $ withTempFile $ \name ->
      withBinaryCFile name AppendMode (`shouldNotBe` nullPtr)

    it "should open a file in ReadWriteMode" $ withTempFile $ \name ->
      withBinaryCFile name ReadWriteMode (`shouldNotBe` nullPtr)
