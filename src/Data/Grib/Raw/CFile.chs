{- |
Module      : Data.Grib.Raw.CFile
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Open and close C streams in Haskell.
-}

module Data.Grib.Raw.CFile
       ( CFilePtr
       , withBinaryCFile
       , openBinaryCFile
       , closeCFile
       , IOMode(..)
       ) where

import Control.Exception ( bracket )
import Control.Monad     ( when )
import Foreign.C         ( CInt, CFile, throwErrno, throwErrnoPathIfNull
                         , withCString )
import System.IO         ( IOMode(..) )


#include <stdio.h>

-- |A pointer to a C stream (FILE).
{#pointer *FILE as CFilePtr -> CFile #}

-- This comment is inserted to help Haddock keep all docs.

-- |A constant indicating end of file.
eof :: CInt
eof = {#const EOF #}

-- FILE *fopen(const char *path, const char *mode);
--
-- This function is not macro expanded since we want to use the given
-- filename in the return value marshaller 'throwErrnoPathIfNull'.
--
-- |Like 'System.IO.openBinaryFile', but return a 'CFilePtr' instead of a
-- file 'System.IO.Handle'.
openBinaryCFile :: FilePath -> IOMode -> IO CFilePtr
openBinaryCFile name mode =
  withCString name     $ \c_name ->
  withCString mode_str $ \c_mode ->
  throwErrnoPathIfNull "openBinaryCFile" name $
    {#call unsafe fopen #} c_name c_mode
  where mode_str = case mode of
          ReadMode      -> "rb"
          WriteMode     -> "wb"
          AppendMode    -> "ab"
          ReadWriteMode -> "r+b"

-- |Close an open 'Foreign.C.CFile'.
{#fun unsafe fclose as closeCFile { `CFilePtr' } -> `()' checkStatus*- #}
  where checkStatus r = when (r == eof) $ throwErrno "closeCFile"

-- |Like 'System.IO.withBinaryFile', but use a 'CFilePtr' instead of a file
-- 'System.IO.Handle'.
withBinaryCFile :: FilePath -> IOMode -> (CFilePtr -> IO a) -> IO a
withBinaryCFile name mode = bracket (openBinaryCFile name mode) closeCFile
