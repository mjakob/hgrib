{- |
Module      : Data.Grib.Raw.Error
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Error codes used by GRIB API.
-}

{-# OPTIONS_HADDOCK hide #-}

module Data.Grib.Raw.Error ( ErrorCode(..) ) where


#include <grib_api.h>

-- |Error codes returned by the grib_api functions.
{#enum define ErrorCode {
      GRIB_SUCCESS                  as GribSuccess
    , GRIB_END_OF_FILE              as GribEndOfFile
    , GRIB_INTERNAL_ERROR           as GribInternalError
    , GRIB_BUFFER_TOO_SMALL         as GribBufferTooSmall
    , GRIB_NOT_IMPLEMENTED          as GribNotImplemented
    , GRIB_7777_NOT_FOUND           as Grib7777NotFound
    , GRIB_ARRAY_TOO_SMALL          as GribArrayTooSmall
    , GRIB_FILE_NOT_FOUND           as GribFileNotFound
    , GRIB_CODE_NOT_FOUND_IN_TABLE  as GribCodeNotFoundInTable
    , GRIB_WRONG_ARRAY_SIZE         as GribWrongArraySize
    , GRIB_NOT_FOUND                as GribNotFound
    , GRIB_IO_PROBLEM               as GribIoProblem
    , GRIB_INVALID_MESSAGE          as GribInvalidMessage
    , GRIB_DECODING_ERROR           as GribDecodingError
    , GRIB_ENCODING_ERROR           as GribEncodingError
    , GRIB_NO_MORE_IN_SET           as GribNoMoreInSet
    , GRIB_GEOCALCULUS_PROBLEM      as GribGeocalculusProblem
    , GRIB_OUT_OF_MEMORY            as GribOutOfMemory
    , GRIB_READ_ONLY                as GribReadOnly
    , GRIB_INVALID_ARGUMENT         as GribInvalidArgument
    , GRIB_NULL_HANDLE              as GribNullHandle
    , GRIB_INVALID_SECTION_NUMBER   as GribInvalidSectionNumber
    , GRIB_VALUE_CANNOT_BE_MISSING  as GribValueCannotBeMissing
    , GRIB_WRONG_LENGTH             as GribWrongLength
    , GRIB_INVALID_TYPE             as GribInvalidType
    , GRIB_WRONG_STEP               as GribWrongStep
    , GRIB_WRONG_STEP_UNIT          as GribWrongStepUnit
    , GRIB_INVALID_FILE             as GribInvalidFile
    , GRIB_INVALID_GRIB             as GribInvalidGrib
    , GRIB_INVALID_INDEX            as GribInvalidIndex
    , GRIB_INVALID_ITERATOR         as GribInvalidIterator
    , GRIB_INVALID_KEYS_ITERATOR    as GribInvalidKeysIterator
    , GRIB_INVALID_NEAREST          as GribInvalidNearest
    , GRIB_INVALID_ORDERBY          as GribInvalidOrderby
    , GRIB_MISSING_KEY              as GribMissingKey
    , GRIB_OUT_OF_AREA              as GribOutOfArea
    , GRIB_CONCEPT_NO_MATCH         as GribConceptNoMatch
    , GRIB_NO_DEFINITIONS           as GribNoDefinitions
    , GRIB_WRONG_TYPE               as GribWrongType
    , GRIB_END                      as GribEnd
    , GRIB_NO_VALUES                as GribNoValues
    , GRIB_WRONG_GRID               as GribWrongGrid
    , GRIB_END_OF_INDEX             as GribEndOfIndex
    , GRIB_NULL_INDEX               as GribNullIndex
    , GRIB_PREMATURE_END_OF_FILE    as GribPrematureEndOfFile
    , GRIB_INTERNAL_ARRAY_TOO_SMALL as GribInternalArrayTooSmall
    , GRIB_MESSAGE_TOO_LARGE        as GribMessageTooLarge
    , GRIB_CONSTANT_FIELD           as GribConstantField
    , GRIB_SWITCH_NO_MATCH          as GribSwitchNoMatch
    , GRIB_UNDERFLOW                as GribUnderflow
    , GRIB_MESSAGE_MALFORMED        as GribMessageMalformed
    , GRIB_CORRUPTED_INDEX          as GribCorruptedIndex
    , GRIB_INVALID_BPV              as GribInvalidBpv
    , GRIB_DIFFERENT_EDITION        as GribDifferentEdition
    , GRIB_VALUE_DIFFERENT          as GribValueDifferent
    , GRIB_INVALID_KEY_VALUE        as GribInvalidKeyValue
    } deriving (Eq, Show) #}
