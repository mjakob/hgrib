{- |
Module      : Data.Grib.Raw.Test
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Utilities for the unit tests of HGrib's raw interface.
-}

module Data.Grib.Raw.Test
       ( skipIfGribApiVersion
       , safeRemoveFile
       , withGribFile
       , regular1Path
       , regular2Path
       , withRegular1
       , withRegular2
       ) where

import Control.Exception ( tryJust )
import Control.Monad     ( (>=>), guard, void )
import System.Directory  ( removeFile )
import System.IO.Error   ( isDoesNotExistError )
import Test.Hspec        ( SpecWith, expectationFailure )

import Data.Grib.Raw
import Data.Grib.Test    ( regular1Path, regular2Path )


skipIfGribApiVersion :: (Int -> Bool) -> SpecWith a -> SpecWith a
skipIfGribApiVersion p
  | p gribGetApiVersion = const $ return ()
  | otherwise           = id

safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path =
  void $ tryJust (guard . isDoesNotExistError) (removeFile path)

withGribFile :: FilePath -> (GribHandle -> IO ()) -> IO ()
withGribFile name g = withBinaryCFile name ReadMode $
  gribHandleNewFromFile defaultGribContext >=> \h ->
  case h of
   Just h' -> g h'
   Nothing -> expectationFailure $ "no GRIB message found in '" ++ name ++ "'"

withRegular1 :: (GribHandle -> IO ()) -> IO ()
withRegular1 = withGribFile regular1Path

withRegular2 :: (GribHandle -> IO ()) -> IO ()
withRegular2 = withGribFile regular2Path
