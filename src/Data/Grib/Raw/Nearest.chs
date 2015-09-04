{- |
Module      : Data.Grib.Raw.Nearest
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Find the nearest grid points of an arbitrary point.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

module Data.Grib.Raw.Nearest
       ( -- *The GRIB Nearest Iterator
         GribNearest(..)
       , gribNearestNew
       , gribNearestFind
       , gribNearestFindMultiple
       , gribNearestDelete
       , withGribNearest

         -- **Nearest flags
       , GribNearestFlag(..)
       ) where

import Control.Exception (bracket)
import Foreign
import Foreign.C

{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal


#include <grib_api.h>

{#typedef size_t CSize #}

-- typedef struct grib_nearest grib_nearest;
--
-- Unlike grib_iterator and grib_keys_iterator, grib_nearest doesn't
-- seem to hold a reference to the grib_handle.  To keep the api
-- consistent, however, it is still treated analogous to those two
-- data types, i.e., no foreign pointer is used.
--
-- |Grib nearest, structure used to find the nearest points of a
-- latitude longitude point.
{#pointer *grib_nearest as GribNearest newtype #} deriving (Eq, Show)

-- |Filter flags for 'GribNearest'.
{#enum define GribNearestFlag {
      GRIB_NEAREST_SAME_GRID  as GribNearestSameGrid
    , GRIB_NEAREST_SAME_DATA  as GribNearestSameData
    , GRIB_NEAREST_SAME_POINT as GribNearestSamePoint
    } deriving (Eq, Show) #}

-- grib_nearest* grib_nearest_new(grib_handle* h, int* error);
--
-- |Create a new nearest from a handle, using current geometry.
--
-- The returned object needs to be manually deleted with
-- 'gribNearestDelete'.  This is handled automatically by
-- 'withGribNearest'.
{#fun grib_nearest_new as ^ {
              `GribHandle'
    , alloca- `CInt'       checkStatusPtr*-
    } -> `GribNearest' #}

-- int grib_nearest_find(grib_nearest *nearest, grib_handle* h, double inlat,
--                       double inlon, unsigned long flags, double* outlats,
--                       double* outlons, double* values, double* distances,
--                       int* indexes, size_t *len);
--
-- |Find the 4 nearest points of a latitude longitude point.
--
-- The flags are provided to speed up the process of searching. If you
-- are sure that the point you are asking for is not changing from a
-- call to another you can use GRIB_NEAREST_SAME_POINT. The same is
-- valid for the grid. Flags can be used together doing a bitwise
-- OR. The distances are given in kilometres.
{#fun grib_nearest_find as ^ {
                    `GribNearest'
    ,               `GribHandle'
    ,               `Double'
    ,               `Double'
    , fromFlagList  `[GribNearestFlag]'
    , allocaArray4- `[Double]'          peekRealArray4*
    , allocaArray4- `[Double]'          peekRealArray4*
    , allocaArray4- `[Double]'          peekRealArray4*
    , allocaArray4- `[Double]'          peekRealArray4*
    , allocaArray4- `[Int]'             peekIntegralArray4*
    , with4-        `CSize'
    } -> `()' checkStatus*- #}
  where allocaArray4 :: Storable a => (Ptr a -> IO b) -> IO b
        allocaArray4       = allocaArray 4
        peekIntegralArray4 = peekIntegralArray 4
        peekRealArray4     = peekRealArray 4
        with4              = with 4

-- int grib_nearest_delete(grib_nearest *nearest);
--
-- |Frees an nearest from memory.
{#fun grib_nearest_delete as ^ { `GribNearest' } -> `()' checkStatus* #}

-- int grib_nearest_find_multiple(grib_handle* h, int is_lsm, double* inlats,
--                                double* inlons, long npoints, double* outlats,
--                                double* outlons, double* values,
--                                double* distances, int* indexes);
--
-- This function is not macro expanded since the length of the output
-- arguments depend on the length of the input arguments.
--
-- |Find the nearest point of a set of points whose latitudes and
-- longitudes are given in the inlats, inlons arrays respectively.
--
-- If the flag is_lsm is 1 the nearest land point is returned and the
-- grib passed as handle (h) is considered a land sea mask. The land
-- nearest point is the nearest point with land sea mask
-- value>=0.5. If no nearest land points are found the nearest value
-- is returned. If the flag is_lsm is 0 the nearest point is
-- returned. values, distances, indexes (in the "values" array) for
-- the nearest points (ilons,ilats) are returned.
gribNearestFindMultiple :: GribHandle
                        -- ^handle from which geography and data
                        -- values are taken
                        -> Bool
                        -- ^lsm flag (@True@ -> nearest land, @False@
                        -- -> nearest)
                        -> [Double]
                        -- ^latitudes of the points to search for
                        -> [Double]
                        -- ^longitudes of the points to search for
                        -> IO ([Double], [Double], [Double], [Double], [Int])
                        -- ^an IO action that will return a tuple
                        -- @(latitudes, longitudes, values, distances,
                        -- indices)@
gribNearestFindMultiple h lsm ilats ilons =
  withGribHandle h $ \h' ->
  let lsm' = fromBool lsm in
  withArray (map realToFrac ilats) $ \ilats' ->
  withArray (map realToFrac ilons) $ \ilons' ->
  let n  = min (length ilats) (length ilons)
      n' = fromIntegral n in
  allocaArray n $ \olats ->
  allocaArray n $ \olons ->
  allocaArray n $ \vals ->
  allocaArray n $ \dists ->
  allocaArray n $ \is -> do
    cCall h' lsm' ilats' ilons' n' olats olons vals dists is >>= checkStatus
    olats' <- fmap (map realToFrac)   (peekArray n olats)
    olons' <- fmap (map realToFrac)   (peekArray n olons)
    vals'  <- fmap (map realToFrac)   (peekArray n vals)
    dists' <- fmap (map realToFrac)   (peekArray n dists)
    is'    <- fmap (map fromIntegral) (peekArray n is)
    return (olats', olons', vals', dists', is')
  where cCall = {#call grib_nearest_find_multiple as gribNearestFindMultiple'_ #}

-- |Safely create, use and delete a 'GribNearest'.
--
-- This function is an easy alternative over using 'gribNearestNew'
-- and 'gribNearestDelete' directly.
withGribNearest :: GribHandle -> (GribNearest -> IO a) -> IO a
withGribNearest h = bracket (gribNearestNew h) gribNearestDelete
