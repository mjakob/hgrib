{- |
Module      : Data.Grib.Test
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Utilities for the unit tests of HGrib's high-level interface.
-}

module Data.Grib.Test
       ( notAGribPath
       , regular1Path
       , regular2Path
       , testUuidPath
       ) where


notAGribPath :: FilePath
notAGribPath = "test/stage/not_a_grib.txt"

regular1Path :: FilePath
regular1Path = "test/stage/regular_latlon_surface.grib1"

regular2Path :: FilePath
regular2Path = "test/stage/regular_latlon_surface.grib2"

testUuidPath :: FilePath
testUuidPath = "test/stage/test_uuid.grib2"
