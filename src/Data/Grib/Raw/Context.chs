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

         -- * GRIB API Version
       , gribGetApiVersion
       ) where

import Foreign ( nullPtr )


#include <grib_api.h>

-- long grib_get_api_version(void);
--
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

-- grib_context* grib_context_get_default(void);
--
-- |Get the static default context.
--
-- Note that the returned object is different from
-- 'defaultGribContext', since that is just a null pointer and this is
-- a pointer to the real thing.  They should, however, be able to be
-- used interchangeably with all the functions in this package.
{#fun unsafe grib_context_get_default as ^ {} -> `GribContext' #}

-- grib_context* grib_context_new(grib_context* c);
--
-- |Create and allocate a new context from a parent context.
{#fun unsafe grib_context_new as ^ { `GribContext' } -> `GribContext' #}

-- void grib_context_delete(grib_context* c);
--
-- It doesn't seem safe to call this function in general, not even
-- with contexts created by 'gribContextNew' since fields of the
-- default context is first copied (by 'gribContextNew') and then
-- deleted (by this function).
--
-- |Frees the cached definition files of the context.
{#fun unsafe grib_context_delete as ^ { `GribContext' } -> `()' #}

-- void grib_gts_header_on(grib_context* c);
--
-- |Set the gts header mode on.  The GTS headers will be preserved.
{#fun unsafe grib_gts_header_on as ^ { `GribContext' } -> `()' #}

-- void grib_gts_header_off(grib_context* c);
--
-- |Set the gts header mode off.  The GTS headers will be deleted.
{#fun unsafe grib_gts_header_off as ^ { `GribContext' } -> `()' #}

-- void grib_gribex_mode_on(grib_context* c);
--
-- |Set the gribex mode on.  Grib files will be compatible with
-- gribex.
{#fun unsafe grib_gribex_mode_on as ^ { `GribContext' } -> `()' #}

-- int grib_get_gribex_mode(grib_context* c);
--
-- |Get the gribex mode.
{#fun unsafe grib_get_gribex_mode as ^ { `GribContext' } -> `Bool' #}

-- void grib_gribex_mode_off(grib_context* c);
--
-- |Set the gribex mode off.  Grib files won't be always compatible
-- with gribex.
{#fun unsafe grib_gribex_mode_off as ^ { `GribContext' } -> `()' #}

-- void grib_multi_support_on(grib_context* c);
--
-- |Turn on support for multiple fields in single grib messages.
{#fun unsafe grib_multi_support_on as ^ { `GribContext' } -> `()' #}

-- void grib_multi_support_off(grib_context* c);
--
-- |Turn off support for multiple fields in single grib messages.
{#fun unsafe grib_multi_support_off as ^ { `GribContext' } -> `()' #}
