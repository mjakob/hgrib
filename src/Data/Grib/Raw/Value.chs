{- |
Module      : Data.Grib.Raw.Value
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Access GRIB header and data values.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

module Data.Grib.Raw.Value
       ( -- * Get values
         gribGetLong
       , gribGetDouble
       , gribGetLongArray
       , gribGetDoubleArray
       , gribGetDoubleElement
       , gribGetDoubleElements
       , gribGetString
       , gribGetBytes

       , gribGetOffset
       , gribGetSize
       , gribGetLength

         -- * Set values
       , gribSetLong
       , gribSetDouble
       , gribSetLongArray
       , gribSetDoubleArray
       , gribSetString
       , gribSetBytes

         -- * Copy values
       , gribCopyNamespace
       ) where

import Foreign
import Foreign.C

{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal

#include <grib_api.h>

{#typedef size_t CSize #}

-- int grib_get_offset(grib_handle* h, const char* key, size_t* offset);
--
-- |Get the number offset of a key in a message if several keys of
-- the same name are present, the offset of the last one is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_get_offset as ^ {
                   `GribHandle'
    , withCString* `Key'
    , alloca-      `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_get_size(grib_handle* h, const char* key, size_t* size);
--
-- |Get the number of coded value from a key, if several keys of the
-- same name are present, the total sum is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_get_size as ^ {
                   `GribHandle'
    , withCString* `Key'
    , alloca-      `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_get_length(grib_handle* h, const char* key, size_t *length);
--
-- |Get the length of the string representation of the key, if several
-- keys of the same name are present, the maximum length is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_get_length as ^ {
                   `GribHandle'
    , withCString* `Key'
    , alloca-      `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_get_long(grib_handle* h, const char* key, long* value);
--
-- |Get a long value from a key, if several keys of the same name are
-- present, the last one is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_get_long as ^ {
                   `GribHandle'
    , withCString* `Key'
    , alloca-      `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_get_double(grib_handle* h, const char* key, double* value);
--
-- |Get a double value from a key, if several keys of the same name
-- are present, the last one is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_get_double as ^ {
                   `GribHandle'
    , withCString* `Key'
    , alloca-      `Double'     peekReal*
    } -> `()' checkStatus*- #}

-- int grib_get_double_element(grib_handle* h, const char* key, int i,
--                             double* value);
--
-- There are no bounds check on i and a too large i often doesn't seem
-- to lead to a segmentation fault.
--
-- |Get as double the i-th element of the "key" array.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
--
-- __WARNING!__ There is no check if the index is out of bounds.
{#fun grib_get_double_element as ^ {
                   `GribHandle'
    , withCString* `Key'
    ,              `Int'
    , alloca-      `Double'     peekReal*
    } -> `()' checkStatus*- #}

-- int grib_get_double_elements(grib_handle* h, const char* key, int* i,
--                              long size, double* value);
--
-- This function is not macro expanded since the length of the output
-- arguments depend on the length of the input arguments.
--
-- |Get as double array the elements of the "key" array whose indexes
-- are listed in the input array i.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
--
-- __WARNING!__ There is no check if the indices are out of bounds.
gribGetDoubleElements :: GribHandle -> Key -> [Int] -> IO [Double]
gribGetDoubleElements h key is =
  withGribHandle h                   $ \h'    ->
  withCString key                    $ \key'  ->
  withArrayLen (map fromIntegral is) $ \n is' ->
  allocaArray n                      $ \ds    -> do
    cCall h' key' is' (fromIntegral n) ds >>= checkStatus
    fmap (map realToFrac) $ peekArray n ds
  where cCall = {#call grib_get_double_elements as gribGetDoubleElements'_ #}

-- int grib_get_string(grib_handle* h, const char* key, char* mesg,
--                     size_t *length);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get a string value from a key, if several keys of the same name
-- are present, the last one is returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribBufferTooSmall@ if the allocated string is
--   too small.
--
-- This function takes an allocated 'CString' and its length, which is
-- used to retrieve the string returned by the IO action.  The
-- 'CString' is not automatically allocated by this function since it
-- could potentially be re-used between multiple calls and the length
-- is not known beforehand.
gribGetString :: GribHandle  -- ^the handle to get the data from
              -> Key         -- ^the key to be searched
              -> CString     -- ^the address of a string where the
                             -- data will be retrieved
              -> Int         -- ^the allocated length of the string
              -> IO String   -- ^an IO action that will return the
                             -- string
gribGetString h key cs n =
  withGribHandle h      $ \h'   ->
  withCString key       $ \key' ->
  with (fromIntegral n) $ \n'   -> do
    cCall h' key' cs n' >>= checkStatus
    fmap (fromIntegral . subtract 1) (peek n') >>= curry peekCStringLen cs
  where cCall = {#call grib_get_string as gribGetString'_ #}

-- int grib_get_bytes(grib_handle* h, const char* key, unsigned char* bytes,
--                    size_t *length);
--
-- |Get raw bytes values from a key.
--
-- If several keys of the same name are present, the last one is
-- returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribArrayTooSmall@ if the allocated array is
--   too small.
{#fun grib_get_bytes as ^ {
                    `GribHandle'
      -- ^the handle to get the data from
    , withCString*  `Key'
      -- ^the key to be searched
    , id            `Bytes'      id
      -- ^the address of a byte array where the data will be retrieved
    , withIntegral* `Int'        peekIntegral*
      -- ^the allocated length of the byte array
    } -> `()' checkStatus*-
      -- ^an IO action that will return the address of the byte array
      -- and the number of bytes retrieved
#}

-- int grib_get_double_array(grib_handle* h, const char* key, double* vals,
--                           size_t *length);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get double array values from a key.
--
-- If several keys of the same name are present, the last one is
-- returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribArrayTooSmall@ if the allocated array is
--   too small.
--
-- This function takes an allocated array and its length, which is
-- used to retrieve the list returned by the IO action.  The array is
-- not automatically allocated by this function since it could
-- potentially be re-used between multiple calls and the length is not
-- known beforehand.
gribGetDoubleArray :: GribHandle   -- ^the handle to get the data from
                   -> Key          -- ^the key to be searched
                   -> Ptr CDouble  -- ^the address of a double array
                                   -- where the data will be retrieved
                   -> Int          -- ^the allocated length of the double
                                   -- array
                   -> IO [Double]  -- ^an IO action that will return the
                                   -- data in a list
gribGetDoubleArray h key ds n = withGribHandle h $ \h' ->
  fmap (map realToFrac) $ getArray (cCall h') key ds n
  where cCall = {#call grib_get_double_array as gribGetDoubleArray'_ #}

-- int grib_get_long_array(grib_handle* h, const char* key, long* vals,
--                         size_t *length);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Get long array values from a key.
--
-- If several keys of the same name are present, the last one is
-- returned.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribArrayTooSmall@ if the allocated array is
--   too small.
--
-- This function takes an allocated array and its length, which is
-- used to retrieve the list returned by the IO action.  The array is
-- not automatically allocated by this function since it could
-- potentially be re-used between multiple calls and the length is not
-- known beforehand.
gribGetLongArray :: GribHandle  -- ^the handle to get the data from
                 -> Key         -- ^the key to be searched
                 -> Ptr CLong   -- ^the address of a long array where
                                -- the data will be retrieved
                 -> Int         -- ^the allocated length of the long
                                -- array
                 -> IO [Int]    -- ^an IO action that will return the
                                -- data in a list
gribGetLongArray h key ls n = withGribHandle h $ \h' ->
  fmap (map fromIntegral) $ getArray (cCall h') key ls n
  where cCall = {#call grib_get_long_array as gribGetLongArray'_ #}

-- int grib_copy_namespace(grib_handle* dest, const char* name,
--                         grib_handle* src);
--
-- |Copy the keys belonging to a given namespace from a source handle
-- to a destination handle.
--
-- This operation may fail with:
--
--   * @isGribException GribNotImplemented@.
{#fun grib_copy_namespace as ^ {
      `GribHandle'                      -- ^destination handle
    , maybeWithCString* `Maybe String'  -- ^namespace (pass @Nothing@
                                        -- to copy all keys)
    , `GribHandle'                      -- ^source handle
    } -> `()' checkStatus*-             -- ^an IO action that will
                                        -- copy the keys
#}

-- int grib_set_long(grib_handle* h, const char* key, long val);
--
-- |Set a long value from a key.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribReadOnly@ if the key is read-only.
{#fun grib_set_long as ^ {
                   `GribHandle'
    , withCString* `Key'
    ,              `Int'
    } -> `()' checkStatus*- #}

-- int grib_set_double(grib_handle* h, const char* key, double val);
--

-- |Set a double value from a key.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribReadOnly@ if the key is read-only.
{#fun grib_set_double as ^ {
                   `GribHandle'
    , withCString* `Key'
    ,              `Double'
  } -> `()' checkStatus*- #}

-- int grib_set_string(grib_handle* h, const char* key, const char* mesg,
--                     size_t *length);
--
-- This function is not macro expanded since an output marshaller
-- would want to return another data type than the corresponding input
-- marshaller accepts.
--
-- |Set a string value from a key and return the actual packed length.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribReadOnly@ if the key is read-only.
gribSetString :: GribHandle -> Key -> String -> IO Int
gribSetString h key msg =
  withGribHandle h                 $ \h'   ->
  withCString key                  $ \key' ->
  withCString msg                  $ \msg' ->
  with (fromIntegral $ length msg) $ \n    ->
    cCall h' key' msg' n >>= checkStatus >> fmap fromIntegral (peek n)
  where cCall = {#call grib_set_string as gribSetString'_ #}

-- int grib_set_bytes(grib_handle* h, const char* key,
--                    const unsigned char* bytes, size_t *length);
--
-- |Set a bytes array from a key and return the actual packed length.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing.
{#fun grib_set_bytes as ^ {
                    `GribHandle'
    , withCString*  `Key'
    , id            `Bytes'
    , withIntegral* `Int' peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_set_double_array(grib_handle* h, const char* key,
--                           const double* vals, size_t length);
--
-- |Set a double array from a key.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribReadOnly@ if the key is read-only.
--
-- __WARNING!__ Strange things seem to happen if an empty list is
-- passed in.
{#fun grib_set_double_array as ^ {
                        `GribHandle'
    , withCString*      `Key'
    , withRealArrayLen* `[Double]'&
    } -> `()' checkStatus*- #}

-- int grib_set_long_array(grib_handle* h, const char* key, const long* vals,
--                         size_t length);
--
-- |Set a long array from a key.
--
-- If several keys of the same name are present, the last one is set.
--
-- This operation may fail with:
--
--   * @isGribException GribNotFound@ if the key is missing; or
--
--   * @isGribException GribReadOnly@ if the key is read-only.
{#fun grib_set_long_array as ^ {
                            `GribHandle'
    , withCString*          `Key'
    , withIntegralArrayLen* `[Int]'&
    } -> `()' checkStatus*- #}
