{- |
Module      : Data.Grib.Exception
Description : Exceptions for HGrib
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Exceptions for HGrib.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Grib.Exception
       (-- *GRIB Exceptions
         GribException(..)

         -- **Predicates
       , isAnyGribException
       , isGribException
       , isNullPtrReturned

         -- **Error codes
       , ErrorCode(..)
       ) where

import Control.Exception ( Exception )
import Data.Typeable     ( Typeable )

import Data.Grib.Raw.Error


-- |An exception carrying an 'ErrorCode' or representing a returned
-- null pointer.
data GribException = GribException ErrorCode
                   | NullPtrReturned
                   deriving (Show, Typeable)

instance Exception GribException

-- |True for any 'GribException'.
isAnyGribException :: GribException -> Bool
isAnyGribException = const True

-- |True if a 'GribException' carries the given 'ErrorCode'.
isGribException :: ErrorCode -> GribException -> Bool
isGribException code (GribException code') = code' == code
isGribException _    NullPtrReturned       = False

-- |True for 'NullPtrReturned'.
isNullPtrReturned :: GribException -> Bool
isNullPtrReturned NullPtrReturned = True
isNullPtrReturned _               = False
