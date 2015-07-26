{- |
Module      : Data.Grib.Raw.Types
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Data types used to marshal parameters between C and Haskell.
-}

module Data.Grib.Raw.Types
       ( Bytes
       , Key
       , Message
       ) where

import Foreign
import Foreign.C


-- |A pointer to a number of bytes in memory.
type Bytes = Ptr CUChar

-- |A key representing one or more consecutive octets in a GRIB
-- message.
type Key = String

-- |A GRIB message stored in memory.
type Message = Ptr ()
