{- |
Module      : Data.Grib.Raw.Context
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

The context is a long life configuration object of the grib_api.  It
is used to define special allocation and free routines or to set
special grib_api behaviours and variables.

Most of the documentation herein was copied from the official
documentation of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index grib_api>.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Grib.Raw.Context
       ( -- * The GRIB Context
         GribContext(..)
       , defaultGribContext
       , gribContextGetDefault
       , gribContextNew
       , gribContextDelete
       , gribContextSetMemoryProc
       , gribContextSetPersistentMemoryProc
       , gribContextSetBufferMemoryProc
       , gribContextSetPrintProc
       , gribContextSetLoggingProc

         -- ** Control GTS Mode
       , gribGtsHeaderOn
       , gribGtsHeaderOff

         -- ** Control Gribex Mode
       , gribGribexModeOn
       , gribGribexModeOff
       , gribGetGribexMode

         -- ** Control Multi-field Support
       , gribMultiSupportOn
       , gribMultiSupportOff

         -- * Foreign Function Type Definitions
       , GribFreeProc
       , GribMallocProc
       , GribReallocProc
       , GribLogProc
       , GribPrintProc
       , GribDataReadProc
       , GribDataWriteProc
       , GribDataTellProc
       , GribDataSeekProc
       , GribDataEofProc

         -- * GRIB API Version
       , gribGetApiVersion
       ) where

import Foreign
import Foreign.C


#include <grib_api.h>

-- |Get the current version of GRIB API as an integer.
--
-- The major version is multiplied by 10000, the minor by 100 and then
-- they are summed together with the revision version to form the
-- integer. For example, version 1.13.1 would be 11301.
{#fun pure unsafe grib_get_api_version as ^ {} -> `Int' #}

-- This comment is inserted to help Haddock keep all docs.

-- typedef struct grib_context grib_context;
--
-- It doesn't seem like grib_context_delete() can be used in general,
-- hence no foreign pointer.
--
-- |The context is a long life configuration object of the grib_api. It is
-- used to define special allocation and free routines or to set
-- special grib_api behaviours and variables.
{#pointer *grib_context as GribContext newtype #} deriving (Eq, Show)

-- |A 'GribContext' containing a null pointer which makes the
-- functions receiving it use the default grib context.
defaultGribContext :: GribContext
defaultGribContext = GribContext nullPtr

-- typedef void (*grib_free_proc)(const grib_context* c, void* data);
--
-- |Grib free procedure, format of a procedure referenced in the
-- context that is used to free memory.
{#pointer grib_free_proc as GribFreeProc #}

-- typedef void* (*grib_malloc_proc)(const grib_context* c, size_t length);
--
-- |Grib malloc procedure, format of a procedure referenced in the
-- context that is used to allocate memory.
{#pointer grib_malloc_proc as GribMallocProc #}

-- typedef void* (*grib_realloc_proc)(const grib_context* c, void* data,
--                                    size_t length);
--
-- |Grib realloc procedure, format of a procedure referenced in the
-- context that is used to reallocate memory.
{#pointer grib_realloc_proc as GribReallocProc #}

-- typedef void (*grib_log_proc)(const grib_context* c, int level,
--                               const char* mesg);
--
-- |Grib loc proc, format of a procedure referenced in the context
-- that is used to log internal messages.
{#pointer grib_log_proc as GribLogProc #}

-- typedef void (*grib_print_proc)(const grib_context* c, void* descriptor,
--                                 const char* mesg);
--
-- |Grib print proc, format of a procedure referenced in the context
-- that is used to print external messages.
{#pointer grib_print_proc as GribPrintProc #}

-- typedef size_t (*grib_data_read_proc)(const grib_context* c, void *ptr,
--                                       size_t size, void *stream);
--
-- |Grib data read proc, format of a procedure referenced in the
-- context that is used to read from a stream in a resource.
{#pointer grib_data_read_proc as GribDataReadProc #}

-- typedef size_t (*grib_data_write_proc)(const grib_context* c, const void *ptr,
--                                        size_t size,  void *stream);
--
-- |Grib data read write, format of a procedure referenced in the
-- context that is used to write to a stream from a resource.
{#pointer grib_data_write_proc as GribDataWriteProc #}

-- typedef off_t (*grib_data_tell_proc)(const grib_context* c, void *stream);
--
-- |Grib data tell, format of a procedure referenced in the context
-- that is used to tell the current position in a stream.
{#pointer grib_data_tell_proc as GribDataTellProc #}

-- typedef off_t (*grib_data_seek_proc)(const grib_context* c, off_t offset,
--                                      int whence, void *stream);
--
-- |Grib data seek, format of a procedure referenced in the context
-- that is used to seek the current position in a stream.
{#pointer grib_data_seek_proc as GribDataSeekProc #}

-- typedef int (*grib_data_eof_proc)(const grib_context* c, void *stream);
--
-- |Grib data eof, format of a procedure referenced in the context
-- that is used to test end of file.
{#pointer grib_data_eof_proc as GribDataEofProc #}

-- grib_context* grib_context_get_default(void);
--
-- |Get the static default context.
--
-- Note that the returned object is different from
-- 'defaultGribContext', since that is just a null pointer and this is
-- a pointer to the real thing.  They should, however, be able to be
-- used interchangeably with all the functions in this package.
{#fun grib_context_get_default as ^ {} -> `GribContext' #}

-- grib_context* grib_context_new(grib_context* c);
--
-- |Create and allocate a new context from a parent context.
{#fun grib_context_new as ^ { `GribContext' } -> `GribContext' #}

-- void grib_context_delete(grib_context* c);
--
-- It doesn't seem safe to call this function in general, not even
-- with contexts created by 'gribContextNew' since fields of the
-- default context is first copied (by 'gribContextNew') and then
-- deleted (by this function).
--
-- |Frees the cached definition files of the context.
{#fun grib_context_delete as ^ { `GribContext' } -> `()' #}

-- void grib_gts_header_on(grib_context* c);
--
-- |Set the gts header mode on.  The GTS headers will be preserved.
{#fun grib_gts_header_on as ^ { `GribContext' } -> `()' #}

-- void grib_gts_header_off(grib_context* c);
--
-- |Set the gts header mode off.  The GTS headers will be deleted.
{#fun grib_gts_header_off as ^ { `GribContext' } -> `()' #}

-- void grib_gribex_mode_on(grib_context* c);
--
-- |Set the gribex mode on.  Grib files will be compatible with
-- gribex.
{#fun grib_gribex_mode_on as ^ { `GribContext' } -> `()' #}

-- int grib_get_gribex_mode(grib_context* c);
--
-- |Get the gribex mode.
{#fun grib_get_gribex_mode as ^ { `GribContext' } -> `Bool' #}

-- void grib_gribex_mode_off(grib_context* c);
--
-- |Set the gribex mode off.  Grib files won't be always compatible
-- with gribex.
{#fun grib_gribex_mode_off as ^ { `GribContext' } -> `()' #}

-- void grib_context_set_memory_proc(grib_context* c, grib_malloc_proc griballoc,
--                                   grib_free_proc gribfree,
--                                   grib_realloc_proc gribrealloc);
--
-- |Sets memory procedures of the context.
{#fun grib_context_set_memory_proc as ^ {
      `GribContext'
    , `GribMallocProc'
    , `GribFreeProc'
    , `GribReallocProc'
    } -> `()' #}

-- void grib_context_set_persistent_memory_proc(grib_context* c,
--                                              grib_malloc_proc griballoc,
--                                              grib_free_proc gribfree);
--
-- |Sets memory procedures of the context for persistent data.
{#fun grib_context_set_persistent_memory_proc as ^ {
      `GribContext'
    , `GribMallocProc'
    , `GribFreeProc'
    } -> `()' #}

-- void grib_context_set_buffer_memory_proc(grib_context* c,
--                                          grib_malloc_proc griballoc,
--                                          grib_free_proc gribfree,
--                                          grib_realloc_proc gribrealloc);
--
-- |Sets memory procedures of the context for large buffers.
{#fun grib_context_set_buffer_memory_proc as ^ {
      `GribContext'
    , `GribMallocProc'
    , `GribFreeProc'
    , `GribReallocProc'
    } -> `()' #}

-- void grib_context_set_print_proc(grib_context* c, grib_print_proc printp);
--
-- |Sets the context printing procedure used for user interaction.
{#fun grib_context_set_print_proc as ^ {
      `GribContext'
    , `GribPrintProc'
    } -> `()' #}

-- void grib_context_set_logging_proc(grib_context* c, grib_log_proc logp);
--
-- |Sets the context logging procedure used for system (warning,
-- errors, infos ...) messages.
{#fun grib_context_set_logging_proc as ^ {
      `GribContext'
    , `GribLogProc'
    } -> `()' #}

-- void grib_multi_support_on(grib_context* c);
--
-- |Turn on support for multiple fields in single grib messages.
{#fun grib_multi_support_on as ^ { `GribContext' } -> `()' #}

-- void grib_multi_support_off(grib_context* c);
--
-- |Turn off support for multiple fields in single grib messages.
{#fun grib_multi_support_off as ^ { `GribContext' } -> `()' #}
