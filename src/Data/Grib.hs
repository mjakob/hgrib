{- |
Module      : Data.Grib
Description : High-level GRIB library.
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable
-}

module Data.Grib ( -- *The GRIB Monad
                   GribIO
                 , runGribIO
                 , runGribIO_
                 , GribEnv

                   -- **Get values
                   --
                   -- |These operations may fail with:
                   --
                   --  * 'isGribException' 'GribNotFound' if the key
                   --    is missing.
                 , getDouble
                 , getLong
                 , getString
                 , getValues

                   -- **Set values
                   --
                   -- |These operations may fail with:
                   --
                   --  * 'isGribException' 'GribNotFound' if the key
                   --    is missing; or
                   --
                   --  * 'isGribException' 'GribReadOnly' if the key
                   --    is read-only.
                 , setDouble
                 , setLong
                 , setString
                 , setValues

                   -- **Utilities
                 , getFilename
                 , getIndex
                 , getHandle
                 , liftIO
                 ) where

import Control.Monad              ( void )
import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import Foreign                    ( allocaArray, allocaBytes )

-- Hack to have Applicative in base < 4.8 but avoid warning in base >= 4.8:
import Control.Applicative
import Prelude

import Data.Grib.Raw


-- Helper that extracts a list of grib handles from a file.
handles :: FilePath -> IO [GribHandle]
handles path = withBinaryCFile path ReadMode go
  where go file = gribHandleNewFromFile defaultGribContext file >>=
                  maybe (return []) (\h' -> (h' :) <$> go file)

-- |The reader environment of 'GribIO' containing the current
-- filename, a 'GribHandle', and its index in the file.
data GribEnv = GribEnv
               { filename :: FilePath
               , index    :: Int
               , handle   :: GribHandle
               }

-- Helper that generates a list of environments for GribIO.
envs :: FilePath -> IO [GribEnv]
envs path = fmap (zipWith3 GribEnv (repeat path) [0..]) (handles path)

-- |The 'GribIO' monad is a 'ReaderT' monad transformer over the
-- 'IO' monad with a 'GribEnv' environment.
type GribIO = ReaderT GribEnv IO

-- |Run an action on each GRIB message in a file and collect the
-- results.
--
-- This operation may fail with:
--
--   * any 'IOError' raised by 'openBinaryCFile';
--
--   * any 'Data.Grib.Exception.GribError' raised by
--   'gribHandleNewFromFile'; or
--
--   * any other exception raised by the given 'GribIO' action.
runGribIO :: FilePath  -- ^a path to a GRIB file
          -> GribIO a  -- ^an action to take on each GRIB message in the file
          -> IO [a]    -- ^the results of the actions
runGribIO path m = envs path >>= mapM (runReaderT m)

-- |Like 'runGribIO', but discard the results.
runGribIO_ :: FilePath -> GribIO a -> IO ()
runGribIO_ path m = envs path >>= mapM_ (runReaderT m)

-- |Return the name of the file being read.
getFilename :: GribIO FilePath
getFilename = fmap filename ask

-- |Return the zero-based index of the current message in the file.
getIndex :: GribIO Int
getIndex = fmap index ask

-- |Return the current 'GribHandle' for use with the 'Data.Grib.Raw'
-- GRIB API bindings.
getHandle :: GribIO GribHandle
getHandle = fmap handle ask

-- |Get the value for a key as a float.
getDouble :: Key -> GribIO Double
getDouble key = getHandle >>= liftIO . flip gribGetDouble key

-- |Get the value for a key as an integer.
getLong :: Key -> GribIO Int
getLong key = getHandle >>= liftIO . flip gribGetLong key

-- |Get the value for a key as a string.
getString :: Key -> GribIO String
getString key = getHandle >>= \h -> do
  n <- liftIO . gribGetLength h $ key
  liftIO . allocaBytes n $ \bufr -> gribGetString h key bufr n

-- |Get the data values of the GRIB message as floats.
getValues :: GribIO [Double]
getValues = getHandle >>= \h -> do
  n <- liftIO . gribGetSize h $ key
  liftIO . allocaArray n $ \array -> gribGetDoubleArray h key array n
  where key = "values"

-- Helper for the value setters below.
setGeneric :: (GribHandle -> Key -> a -> IO b) -> Key -> a -> GribIO b
setGeneric setter key value = getHandle >>= \h -> liftIO $ setter h key value

-- |Set the value of a key from a float.
setDouble :: Key -> Double -> GribIO ()
setDouble = setGeneric gribSetDouble

-- |Set the value of a key from an integer.
setLong :: Key -> Int -> GribIO ()
setLong = setGeneric gribSetLong

-- |Set the value of a key from a string.
setString :: Key -> String -> GribIO ()
setString key value = void $ setGeneric gribSetString key value

-- |Set the values of the GRIB message from floats.
setValues :: [Double] -> GribIO ()
setValues = setGeneric gribSetDoubleArray "values"
