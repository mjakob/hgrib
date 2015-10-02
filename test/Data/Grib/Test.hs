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
       ( regular1Path
       , regular2Path
       ) where


regular1Path :: FilePath
regular1Path = "test/stage/regular_latlon_surface.grib1"

regular2Path :: FilePath
regular2Path = "test/stage/regular_latlon_surface.grib2"
