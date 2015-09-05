{- |
Module      : Data.Grib.Raw.Iterator
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Iterate on latitude, longitude, values.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

module Data.Grib.Raw.Iterator
       ( -- *The GRIB Iterator
         GribIterator(..)
       , gribIteratorNew
       , gribIteratorNext
       , gribIteratorPrevious
       , gribIteratorHasNext
       , gribIteratorReset
       , gribIteratorDelete
       , withGribIterator
       ) where

import Control.Exception ( bracket )
import Foreign           ( alloca )

{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal


#include <grib_api.h>

-- typedef struct grib_iterator grib_iterator;
--
-- If the grib_handle used to create a grib_iterator has been freed
-- (garbage collected), deleting the iterator leads to undefined
-- behavior since a reference to the handle is used to access the
-- grib_context that in turn is used to free the memory.  Because of
-- this, we can't use a foreign pointer here.
--
-- |Grib iterator, structure supporting a geographic iteration of
-- values on a grib message.
{#pointer *grib_iterator as GribIterator newtype #} deriving (Eq, Show)

-- grib_iterator* grib_iterator_new(grib_handle* h, unsigned long flags,
--                                  int* error);
--
-- |Create a new iterator from a handle, using current geometry and
-- values.
--
-- The returned iterator needs to be manually deleted with
-- 'gribIteratorDelete'.  However, due to the reason given in that
-- function, 'withGribIterator' should be preferred over this
-- function.
{#fun grib_iterator_new as ^ {
              `GribHandle'
      -- ^the handle from which the iterator will be created
    ,         `Int'
      -- ^flags for future use (ignored)
    , alloca- `CInt'       checkStatusPtr*-
    } -> `GribIterator'
      -- ^an IO action that will return the new iterator
#}

-- int grib_iterator_next(grib_iterator *i, double* lat, double* lon,
--                        double* value);
--
-- |Get the next value from an iterator.
--
-- This function returns a tuple @(status, latitude, longitude,
-- value)@, where @status@ is @True@ if successful and @False@ if no
-- more data is available.
{#fun grib_iterator_next as ^ {
              `GribIterator'
    , alloca- `Double'       peekReal*
    , alloca- `Double'       peekReal*
    , alloca- `Double'       peekReal*
    } -> `Bool' #}

-- int grib_iterator_previous(grib_iterator *i, double* lat, double* lon,
--                            double* value);
--
-- |Like 'gribIteratorNext', but return the previous value instead.
{#fun grib_iterator_previous as ^ {
              `GribIterator'
    , alloca- `Double'       peekReal*
    , alloca- `Double'       peekReal*
    , alloca- `Double'       peekReal*
    } -> `Bool' #}

-- int grib_iterator_has_next(grib_iterator *i);
--
-- |Test procedure for values in an iterator.
{#fun grib_iterator_has_next as ^ { `GribIterator' } -> `Bool' #}

-- int grib_iterator_reset(grib_iterator *i);
--
-- |Reset the iterator.
{#fun grib_iterator_reset as ^ { `GribIterator' } -> `()' checkStatus*- #}

-- int grib_iterator_delete(grib_iterator *i);
--
-- |Frees an iterator from memory.
--
-- If the 'GribHandle' used to create the iterator has been garbage
-- collected by the time this function is called, the behavior is
-- undefined.  Because of this, 'withGribIterator' should be preferred
-- over directly using 'gribIteratorNew' and this function.
{#fun grib_iterator_delete as ^ { `GribIterator' } -> `()' checkStatus*- #}

-- |Safely create, use and delete a 'GribIterator'.
--
-- This function should be preferred over directly using
-- 'gribIteratorNew' and 'gribIteratorDelete'.
withGribIterator :: GribHandle
                 -- ^the handle from which the iterator will be created
                 -> Int
                 -- ^flags for future use (ignored)
                 -> (GribIterator -> IO a)
                 -- ^a function that will be called with the newly
                 -- created iterator
                 -> IO a
                 -- ^the result of the above function
withGribIterator h flags = bracket before after
  where before     = gribIteratorNew h flags
        after iter = withGribHandle h $ \_ -> gribIteratorDelete iter
