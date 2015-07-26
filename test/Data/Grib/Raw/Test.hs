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
       ( safeRemoveFile
       , withGribFile
       , regular1Path
       , regular2Path
       , withRegular1
       , withRegular2
       ) where

import Control.Exception (tryJust)
import Control.Monad     ((>=>), guard, void)
import System.Directory  (removeFile)
import System.IO.Error   (isDoesNotExistError)

import Data.Grib.Raw


safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path =
  void $ tryJust (guard . isDoesNotExistError) (removeFile path)

withGribFile :: FilePath -> (GribHandle -> IO ()) -> IO ()
withGribFile name f = withBinaryCFile name ReadMode $
                      gribHandleNewFromFile defaultGribContext >=> f

regular1Path :: FilePath
regular1Path = "test/stage/regular_latlon_surface.grib1"

regular2Path :: FilePath
regular2Path = "test/stage/regular_latlon_surface.grib2"

withRegular1 :: (GribHandle -> IO ()) -> IO ()
withRegular1 = withGribFile regular1Path

withRegular2 :: (GribHandle -> IO ()) -> IO ()
withRegular2 = withGribFile regular2Path
