{- |
Module      : Data.Grib.Raw.Handle
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

The grib_handle is the structure giving access to parsed grib values
by keys.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

{-# LANGUAGE TupleSections #-}

module Data.Grib.Raw.Handle
       ( -- * The GRIB Handle
         GribHandle(..)
       , gribHandleNewFromFile
       , gribHandleNewFromTemplate
       , gribHandleNewFromSamples
       , gribHandleClone
       , gribWriteMessage
       , withGribHandle

         -- ** Operations on raw messages
       , gribGetMessage
       , gribGetMessageCopy
       , gribHandleNewFromMessage
       , gribHandleNewFromMessageCopy
       , gribHandleNewFromMultiMessage

         -- * The GRIB Multi Field Handle
       , GribMultiHandle(..)
       , gribMultiHandleNew
       , gribMultiHandleAppend
       , gribMultiHandleWrite
       , withGribMultiHandle

         -- * Utilities
       , gribCountInFile
       ) where

import Foreign   ( Ptr, alloca, newForeignPtr, nullPtr, peek, with )
import Foreign.C ( CSize, withCString )

{#import Data.Grib.Raw.CFile #}
{#import Data.Grib.Raw.Context #}
import Data.Grib.Raw.Marshal


#include <grib_api.h>

{#typedef size_t CSize #}
{#default in `Int' [size_t] fromIntegral #}

-- typedef struct grib_handle grib_handle;
--
-- |Grib handle, structure giving access to parsed grib values by
-- keys.
{#pointer *grib_handle as GribHandle
     foreign finalizer grib_handle_delete as gribHandleFinalizer
     newtype #}

instance Eq GribHandle where
  GribHandle f1 == GribHandle f2 = f1 == f2

instance Show GribHandle where
  show (GribHandle f) = "GribHandle " ++ show f

checkHandle :: Ptr GribHandle -> IO GribHandle
checkHandle = checkForeignPtr GribHandle gribHandleFinalizer

-- int grib_count_in_file(grib_context *c, FILE *f, int *n);
--
-- |Counts the messages contained in a file resource.
{#fun unsafe grib_count_in_file as ^ {
              `GribContext'
    ,         `CFilePtr'
    , alloca- `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- grib_handle* grib_handle_new_from_file(grib_context* c, FILE* f, int* error);
--
-- |Create a handle from a file resource.
--
-- The file is read until a message is found. The message is then
-- copied.  If no more messages are found, @Nothing@ is returned.
{#fun unsafe grib_handle_new_from_file as ^ {
              `GribContext'
    ,         `CFilePtr'
    , alloca- `CInt'        checkStatusPtr*-
    } -> `Maybe GribHandle' maybeHandle* #}
  where maybeHandle :: Ptr GribHandle -> IO (Maybe GribHandle)
        maybeHandle p
          | p == nullPtr = return Nothing
          | otherwise    = fmap (Just . GribHandle) fp
          where fp = newForeignPtr gribHandleFinalizer p

-- int grib_write_message(grib_handle* h,const char* file,const char* mode);
--
-- |Write a coded message to a file given its name and the C file mode
-- string, in that order.
{#fun unsafe grib_write_message as ^ {
                   `GribHandle'
    , withCString* `FilePath'
    ,              `String'
    } -> `()' checkStatus*- #}

-- grib_handle* grib_handle_new_from_message(grib_context* c, void* data,
--                                           size_t data_len);
--
-- |Create a handle from a user message in memory.
--
-- The message will not be freed at the end. The message will be
-- copied as soon as a modification is needed.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the message is invalid or a problem is
--   encountered.
--
-- __WARNING!__ This method does not handle a message of zero length
-- gracefully.
{#fun unsafe grib_handle_new_from_message as ^ {
         `GribContext'
    , id `Message'
    ,    `Int'
    } -> `GribHandle' checkHandle* #}

-- grib_handle* grib_handle_new_from_multi_message(grib_context* c,void** data,
--                                                 size_t *data_len,int* error);
--
-- |Create a handle from a user message in memory.
--
-- The message will not be freed at the end. The message will be
-- copied as soon as a modification is needed. This function works
-- also with multi field messages.
{#fun unsafe grib_handle_new_from_multi_message as ^ {
                    `GribContext'
    , with*         `Message'     peek*
    , withIntegral* `Int'         peekIntegral*
    , alloca-       `CInt'        checkStatusPtr*-
    } -> `GribHandle' #}

-- grib_handle* grib_handle_new_from_message_copy(grib_context* c,
--                                                const void* data,
--                                                size_t data_len);
--
-- |Create a handle from a user message.
--
-- The message is copied and will be freed with the handle.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the message is invalid or a problem is
--   encountered.
{#fun unsafe grib_handle_new_from_message_copy as ^ {
         `GribContext'
    , id `Message'
    ,    `Int'
    } -> `GribHandle' checkHandle* #}

-- DEPRECATED grib_handle* grib_handle_new_from_template(grib_context* c,
--                                                       const char* res_name);
--
-- |/This function has been deprecated in GRIB API./
--
-- Create a handle from a read_only template resource.
--
-- The message is copied at the creation of the handle.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the resource is invalid or a problem is
--   encountered.
{#fun unsafe grib_handle_new_from_template as ^ {
      `GribContext'
    , `String'
    } -> `GribHandle' checkHandle* #}

-- grib_handle* grib_handle_new_from_samples(grib_context* c,
--                                           const char* res_name);
--
-- |Create a handle from a message contained in a samples directory.
--
-- The message is copied at the creation of the handle.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the resource is invalid or a problem is
--   encountered.
{#fun unsafe grib_handle_new_from_samples as ^ {
      `GribContext'  -- ^the context from which the handle will be
                     -- created (NULL for default context)
    , `String'       -- ^the resource name
    } -> `GribHandle' checkHandle* #}

-- grib_handle* grib_handle_clone(grib_handle *h);
--
-- |Clone an existing handle using the context of the original handle.
--
-- The message is copied and reparsed.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if the message is invalid or a problem is
--   encountered.
{#fun unsafe grib_handle_clone as ^ {
      `GribHandle'
    } -> `GribHandle' checkHandle* #}

-- typedef struct grib_multi_handle grib_multi_handle;
--
-- |Grib multi field handle, structure used to build multi fields
-- messages.
{#pointer *grib_multi_handle as GribMultiHandle
     foreign finalizer grib_multi_handle_delete as gribMultiHandleFinalizer
     newtype #}

instance Eq GribMultiHandle where
  GribMultiHandle f1 == GribMultiHandle f2 = f1 == f2

instance Show GribMultiHandle where
  show (GribMultiHandle f) = "GribMultiHandle " ++ show f

checkMultiHandle :: Ptr GribMultiHandle -> IO GribMultiHandle
checkMultiHandle = checkForeignPtr GribMultiHandle gribMultiHandleFinalizer

-- grib_multi_handle* grib_multi_handle_new(grib_context* c);
--
-- |Create an empty multi field handle.
--
-- This operation may fail with:
--
--   * @NullPtrReturned@ if a problem is encountered.
{#fun unsafe grib_multi_handle_new as ^ {
    `GribContext'
    } -> `GribMultiHandle' checkMultiHandle* #}

-- int grib_multi_handle_append(grib_handle* h, int start_section,
--                              grib_multi_handle* mh);
--
-- |Append the sections starting with start_section of the message
-- pointed by h at the end of the multi field handle mh.
{#fun unsafe grib_multi_handle_append as ^ {
      `GribHandle'
    , `Int'
    , `GribMultiHandle'
    } -> `()' checkStatus*- #}

-- int grib_multi_handle_write(grib_multi_handle* mh, FILE* f);
--
-- |Write a multi field handle in a file.
{#fun unsafe grib_multi_handle_write as ^ {
    `GribMultiHandle',
    `CFilePtr'
    } -> `()' checkStatus*- #}

-- int grib_get_message(grib_handle* h, const void** message,
--                      size_t *message_length);
--
-- |Getting the message attached to a handle.
{#fun unsafe grib_get_message as ^ {
              `GribHandle'
    , alloca- `Message'    peek*
    , alloca- `Int'        peekIntegral*
    } -> `()' checkStatus*- #}

-- int grib_get_message_copy(grib_handle* h, void* message,
--                           size_t *message_length);
--
-- |Getting a copy of the message attached to a handle.
--
-- This operation may fail with:
--
--   * @isGribException GribBufferTooSmall@ if the allocated message
--   is too small.
{#fun unsafe grib_get_message_copy as ^ {
                    `GribHandle'
      -- ^the grib handle to which the buffer should be returned
    , id            `Message'    id
      -- ^the pointer to the data buffer to be filled
    , withIntegral* `Int'        peekIntegral*
      -- ^the size in number of bytes of the allocated empty message
    } -> `()' checkStatus*-
      -- ^an IO action that will return the pointer to the data buffer
      -- and the number of bytes retrieved
#}
