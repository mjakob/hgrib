{- |
Module      : Data.Grib.Raw.Fieldset
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

This is an undocumented module of
<https://software.ecmwf.int/wiki/display/GRIB/Module+Index GRIB API>.
-}

module Data.Grib.Raw.Fieldset
       ( GribFieldset
       , gribFieldsetNewFromFiles
       , gribFieldsetApplyOrderBy
       , gribFieldsetNextHandle
       , gribFieldsetCount
       , gribFieldsetRewind
       , withGribFieldset
       ) where

import Foreign ( alloca )

{#import Data.Grib.Raw.Context #}
{#import Data.Grib.Raw.Handle #}
import Data.Grib.Raw.Marshal


#include <grib_api.h>

-- typedef struct grib_fieldset grib_fieldset;
{#pointer *grib_fieldset as GribFieldset
    foreign finalizer grib_fieldset_delete as gribFieldsetFinalizer
    newtype #}

-- grib_fieldset *grib_fieldset_new_from_files(grib_context *c,
--                                             char *filenames[], int nfiles,
--                                             char **keys, int nkeys,
--                                             char *where_string,
--                                             char *order_by_string, int *err);
{#fun grib_fieldset_new_from_files as ^ {
                        `GribContext'
    , withCStrings*     `[FilePath]'&
    , withCStrings*     `[Key]'&
    , maybeWithCString* `Maybe String'  -- ^where
    , maybeWithCString* `Maybe String'  -- ^order_by
    , alloca-           `CInt'         checkStatusPtr*-
    } -> `GribFieldset' #}

-- void grib_fieldset_rewind(grib_fieldset* set);
{#fun grib_fieldset_rewind as ^ { `GribFieldset' } -> `()' #}

-- int grib_fieldset_apply_order_by(grib_fieldset* set,
--                                  const char* order_by_string);
{#fun grib_fieldset_apply_order_by as ^ {
      `GribFieldset'
    , `String'        -- ^order_by
    } -> `()' checkStatus*- #}

-- grib_handle* grib_fieldset_next_handle(grib_fieldset* set, int* err);
{#fun grib_fieldset_next_handle as ^ {
              `GribFieldset'
    , alloca- `CInt'         checkStatusPtr*-
    } -> `GribHandle' #}

-- int grib_fieldset_count(grib_fieldset *set);
{#fun pure unsafe grib_fieldset_count as ^ { `GribFieldset' } -> `Int' #}
