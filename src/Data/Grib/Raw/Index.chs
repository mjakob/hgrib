{- |
Module      : Data.Grib.Raw.Index
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

The grib_index is the structure giving indexed access to messages in a
file.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

module Data.Grib.Raw.Index
       ( -- *The GRIB Index
         GribIndex(..)
       , gribIndexNewFromFile
       , gribIndexNew
       , gribIndexAddFile
       , gribIndexWrite
       , gribIndexRead
       , withGribIndex

         -- **Get values from the index
       , gribIndexGetSize
       , gribIndexGetLong
       , gribIndexGetDouble
       , gribIndexGetString

         -- **Select values from the index
       , gribIndexSelectLong
       , gribIndexSelectDouble
       , gribIndexSelectString

         -- **Other functions
       , gribHandleNewFromIndex
       ) where

import Foreign
import Foreign.C

{#import Data.Grib.Raw.Context #}
{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal

#include <grib_api.h>

{#typedef size_t CSize #}

-- typedef struct grib_index grib_index;
--
-- |Index structure to access messages in a file.
{#pointer *grib_index as GribIndex
    foreign finalizer grib_index_delete as gribIndexFinalizer
    newtype #}

-- grib_index* grib_index_new_from_file(grib_context* c, char* filename,
--                                      const char* keys, int *err);
--
-- |Create a new index from a file.
--
-- The file is indexed with the keys in argument.
{#fun grib_index_new_from_file as ^ {
                         `GribContext'
      -- ^context
    , withCString*       `FilePath'
      -- ^name of the file of messages to be indexed
    , withJoinedCString* `[Key]'
      -- ^a list of keys for the index. The type of the key can be
      -- explicitly declared appending :l for long, :d for double, :s
      -- for string to the key name. If the type is not declared
      -- explicitly, the native type is assumed
    , alloca-            `CInt'        checkStatusPtr*-
    } -> `GribIndex'
      -- ^an IO action that will return the new index
#}

-- grib_index* grib_index_new(grib_context* c, const char* keys, int *err);
--
-- |Create a new index based on a set of keys.
{#fun grib_index_new as ^ {
                         `GribContext'
      -- ^context
    , withJoinedCString* `[Key]'
      -- ^a list of keys for the index. The type of the key can be
      -- explicitly declared appending :l for long, :d for double, :s
      -- for string to the key name. If the type is not declared
      -- explicitly, the native type is assumed
    , alloca-            `CInt'        checkStatusPtr*-
    } -> `GribIndex'
      -- ^an IO action that will return the new index
#}

-- int grib_index_add_file(grib_index *index, const char *filename);
--
-- |Indexes the file given in argument in the index given in argument.
{#fun grib_index_add_file as ^ {
                   `GribIndex'
    , withCString* `FilePath'
    } -> `()' checkStatus*- #}

-- int grib_index_write(grib_index *index, const char *filename);
--
-- |Write the index and its messages to file.
{#fun grib_index_write as ^ {
                   `GribIndex'
    , withCString* `FilePath'
    } -> `()' checkStatus*- #}

-- grib_index* grib_index_read(grib_context* c, const char* filename, int *err);
--
-- |Read messages and their index from a file.
{#fun grib_index_read as ^ {
                   `GribContext'
    , withCString* `FilePath'
    , alloca-      `CInt'        checkStatusPtr*-
    } -> `GribIndex' #}

-- int grib_index_get_size(grib_index* index, const char* key, size_t* size);
--
-- |Get the number of distinct values of the key in argument contained
-- in the index.
--
-- The key must belong to the index.
{#fun grib_index_get_size as ^ {
                   `GribIndex'
    , withCString* `Key'
    , alloca-      `Int'       peekIntegral*
  } -> `()' checkStatus*- #}

-- int grib_index_get_long(grib_index* index, const char* key, long* values,
--                         size_t *size);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get the distinct values of the key in argument contained in the index.
--
-- This function is used when the type of the key was explicitly
-- defined as long or when the native type of the key is long.
--
-- This function takes the address of an array that is used to
-- retrieve the values. It could potentially be re-used between
-- multiple calls.
gribIndexGetLong :: GribIndex  -- ^an index created from a file. The
                               -- index must have been created with
                               -- the key in argument.
                 -> Key        -- ^key for which the values are
                               -- returned
                 -> Ptr CLong  -- ^array of values. The array must be
                               -- allocated before entering this
                               -- function and its size must be enough
                               -- to contain all the values.
                 -> Int        -- ^size of the values array
                 -> IO [Int]   -- ^an IO action that will return the
                               -- data in a list
gribIndexGetLong idx key ls n = withGribIndex idx $ \idx' ->
  map fromIntegral <$> getArray (cCall idx') key ls n
  where cCall = {#call grib_index_get_long as gribIndexGetLong'_ #}

-- int grib_index_get_double(grib_index* index, const char* key,
--                           double* values, size_t *size);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get the distinct values of the key in argument contained in the index.
--
-- This function is used when the type of the key was explicitly
-- defined as double or when the native type of the key is double.
--
-- This function takes the address of an array that is used to
-- retrieve the values. It could potentially be re-used between
-- multiple calls.
gribIndexGetDouble :: GribIndex    -- ^an index created from a
                                   -- file. The index must have been
                                   -- created with the key in
                                   -- argument.
                   -> Key          -- ^key for which the values are
                                   -- returned
                   -> Ptr CDouble  -- ^array of values. The array must
                                   -- be allocated before entering
                                   -- this function and its size must
                                   -- be enough to contain all the
                                   -- values.
                   -> Int          -- ^size of the values array
                   -> IO [Double]  -- ^an IO action that will return
                                   -- the data in a list
gribIndexGetDouble idx key ds n = withGribIndex idx $ \idx' ->
  map realToFrac <$> getArray (cCall idx') key ds n
  where cCall = {#call grib_index_get_double as gribIndexGetDouble'_ #}

-- int grib_index_get_string(grib_index* index, const char* key, char** values,
--                           size_t *size);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get the distinct values of the key in argument contained in the
-- index.
--
-- This function is used when the type of the key was explicitly
-- defined as string or when the native type of the key is string.
--
-- This function takes the address of an array that is used to
-- retrieve the values. It could potentially be re-used between
-- multiple calls.
--
-- __WARNING!__ This function seem to leak memory unless the returned
-- strings in values are eventually freed with the grib context's
-- free_mem function.
gribIndexGetString :: GribIndex    -- ^an index created from a
                                   -- file. The index must have been
                                   -- created with the key in
                                   -- argument.
                   -> Key          -- ^key for which the values are
                                   -- returned
                   -> Ptr CString  -- ^array of values. The array must
                                   -- be allocated before entering
                                   -- this function and its size must
                                   -- be enough to contain all the
                                   -- values.
                   -> Int          -- ^size of the values array
                   -> IO [String]  -- ^an IO action that will return
                                   -- the data in a list
gribIndexGetString idx key ss n = withGribIndex idx $ \idx' ->
  getArray (cCall idx') key ss n >>= mapM peekCString
  where cCall = {#call grib_index_get_string as gribIndexGetString'_ #}

-- int grib_index_select_long(grib_index* index, const char* key, long value);
--
-- |Select the message subset with key==value.
--
-- The index must have been created with the key in argument. The
-- value is a long. The key must have been created with long type or
-- have long as native type if the type was not explicitly defined in
-- the index creation.
{#fun grib_index_select_long as ^ {
                   `GribIndex'
    , withCString* `Key'
    ,              `Int'
    } -> `()' checkStatus*- #}

-- int grib_index_select_double(grib_index* index, const char* key,
--                              double value);
--
-- |Select the message subset with key==value.
--
-- The index must have been created with the key in argument. The
-- value is a double. The key must have been created with double type
-- or have double as native type if the type was not explicitly
-- defined in the index creation.
{#fun grib_index_select_double as ^ {
                   `GribIndex'
    , withCString* `Key'
    ,              `Double'
    } -> `()' checkStatus*- #}

-- int grib_index_select_string(grib_index* index, const char* key, char* value);
--
-- |Select the message subset with key==value.
--
-- The index must have been created with the key in argument. The
-- value is a string. The key must have been created with string type
-- or have string as native type if the type was not explicitly
-- defined in the index creation.
{#fun grib_index_select_string as ^ {
                   `GribIndex'
    , withCString* `Key'
    ,              `String'
    } -> `()' checkStatus*- #}

-- grib_handle* grib_handle_new_from_index(grib_index* index,int *err);
--
-- |Create a new handle from an index after having selected the key values.
--
-- All the keys belonging to the index must be selected before calling
-- this function. Successive calls to this function will return all
-- the handles compatible with the constraints defined selecting the
-- values of the index keys.
--
-- This operation may fail with:
--
--   * @isGribException GribEndOfIndex@ when no more handles are
--   available from the index.
{#fun grib_handle_new_from_index as ^ {
              `GribIndex'
    , alloca- `CInt'      checkStatusPtr*-
    } -> `GribHandle' #}
