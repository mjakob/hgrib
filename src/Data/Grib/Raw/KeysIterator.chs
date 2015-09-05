{- |
Module      : Data.Grib.Raw.KeysIterator
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

The keys iterator is designed to get the key names defined in a
message.  Key names on which the iteration is carried out can be
filtered through their attributes or by the namespace they belong to.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

module Data.Grib.Raw.KeysIterator
       ( -- *The GRIB Keys Iterator
         GribKeysIterator(..)
       , gribKeysIteratorNew
       , gribKeysIteratorNext
       , gribKeysIteratorGetName
       , gribKeysIteratorRewind
       , gribKeysIteratorDelete
       , withGribKeysIterator

         -- **Iterator flags
       , GribKeysIteratorFlag(..)
       , gribKeysIteratorSetFlags
       ) where

import Control.Exception ( bracket, throw )
import Foreign           ( nullPtr )
import Foreign.C         ( peekCString )

import Data.Grib.Raw.Exception
{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal


#include <grib_api.h>

-- |Filter flags for 'GribKeysIterator'.
{#enum define GribKeysIteratorFlag {
      GRIB_KEYS_ITERATOR_ALL_KEYS              as GribKeysIteratorAllKeys
    , GRIB_KEYS_ITERATOR_SKIP_READ_ONLY        as GribKeysIteratorSkipReadOnly
    , GRIB_KEYS_ITERATOR_SKIP_OPTIONAL         as GribKeysIteratorSkipOptional
    , GRIB_KEYS_ITERATOR_SKIP_EDITION_SPECIFIC as
         GribKeysIteratorSkipEditionSpecific
    , GRIB_KEYS_ITERATOR_SKIP_CODED            as GribKeysIteratorSkipCoded
    , GRIB_KEYS_ITERATOR_SKIP_COMPUTED         as GribKeysIteratorSkipComputed
    , GRIB_KEYS_ITERATOR_SKIP_DUPLICATES       as GribKeysIteratorSkipDuplicates
    , GRIB_KEYS_ITERATOR_SKIP_FUNCTION         as GribKeysIteratorSkipFunction
    } deriving (Eq, Show) #}

-- typedef struct grib_keys_iterator grib_keys_iterator;
--
-- If the grib_handle used to create a grib_keys_iterator has been
-- freed (garbage collected), deleting the iterator leads to undefined
-- behavior since a reference to the handle is used to access the
-- grib_context that in turn is used to free the memory.  Because of
-- this, we can't use a foreign pointer here.
--
-- |Grib keys iterator. Iterator over keys.
{#pointer *grib_keys_iterator as GribKeysIterator newtype #} deriving (Eq, Show)

-- grib_keys_iterator* grib_keys_iterator_new(grib_handle* h,
--                                            unsigned long filter_flags,
--                                            const char* name_space);
--
-- |Create a new iterator from a valid and initialized handle.
--
-- The returned iterator needs to be manually deleted with
-- 'gribKeysIteratorDelete'.  However, due to the reason given in that
-- function, 'withGribKeysIterator' should be preferred over this
-- function.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the handle is invalid or the memory
--   allocation fails.
{#fun grib_keys_iterator_new as ^ {
                        `GribHandle'
      -- ^the handle whose keys you want to iterate
    , fromFlagList      `[GribKeysIteratorFlag]'
      -- ^flags to filter out some of the keys through their
      -- attributes
    , maybeWithCString* `Maybe String'
      -- ^if not @Nothing@, the iteration is carried out only on keys
      -- belonging to the namespace passed
    } -> `GribKeysIterator' checkKeysIterator
      -- ^an IO action that will return the new iterator
#}
  where checkKeysIterator kiter@(GribKeysIterator ptr)
          | ptr == nullPtr = throw NullPtrReturned
          | otherwise      = kiter

-- int grib_keys_iterator_next(grib_keys_iterator *kiter);
--
-- |Try to step to the next key and return @True@ if successful.
{#fun grib_keys_iterator_next as ^ { `GribKeysIterator' } -> `Bool' #}

-- const char* grib_keys_iterator_get_name(grib_keys_iterator *kiter);
--
-- |Get the key name from the iterator.
{#fun grib_keys_iterator_get_name as ^ {
    `GribKeysIterator'
    } -> `Key' peekCString* #}

-- int grib_keys_iterator_delete(grib_keys_iterator* kiter);
--
-- |Delete the iterator.
--
-- If the 'GribHandle' used to create the iterator has been garbage
-- collected by the time this function is called, the behavior is
-- undefined.  Because of this, 'withGribKeysIterator' should be
-- preferred over directly using 'gribKeysIteratorNew' and this
-- function.
{#fun grib_keys_iterator_delete as ^ {
    `GribKeysIterator'
    } -> `()' checkStatus*- #}

-- int grib_keys_iterator_rewind(grib_keys_iterator* kiter);
--
-- |Rewind the iterator.
{#fun grib_keys_iterator_rewind as ^ {
    `GribKeysIterator'
    } -> `()' checkStatus*- #}

-- int grib_keys_iterator_set_flags(grib_keys_iterator *kiter,
--                                  unsigned long flags);
--
-- |Update the flags of the iterator.
{#fun grib_keys_iterator_set_flags as ^ {
                   `GribKeysIterator'
    , fromFlagList `[GribKeysIteratorFlag]'
  } -> `()' checkStatus*- #}

-- |Safely create, use and delete a 'GribKeysIterator'.
--
-- This function should be preferred over directly using
-- 'gribKeysIteratorNew' and 'gribKeysIteratorDelete'.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the handle is invalid or the memory
--   allocation fails.
withGribKeysIterator :: GribHandle
                     -- ^the handle whose keys you want to iterate
                     -> [GribKeysIteratorFlag]
                     -- ^flags to filter out some of the keys through
                     -- their attributes
                     -> Maybe String
                     -- ^if not @Nothing@, the iteration is carried
                     -- out only on keys belonging to the namespace
                     -- passed
                     -> (GribKeysIterator -> IO a)
                     -- ^a function that will be called with the newly
                     -- created iterator
                     -> IO a
                     -- ^the result of the above function
withGribKeysIterator h flags ns = bracket before after
  where before      = gribKeysIteratorNew h flags ns
        after kiter = withGribHandle h $ \_ -> gribKeysIteratorDelete kiter
