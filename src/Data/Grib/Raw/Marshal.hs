{-
Module      : Data.Grib.Raw.Marshal
Copyright   : (c) Mattias Jakobsson 2015
License     : GPL-3

Maintainer  : mjakob422@gmail.com
Stability   : unstable
Portability : portable

Functions to marshal parameters between C and Haskell.
-}

module Data.Grib.Raw.Marshal
       ( module Data.Grib.Raw.Types

       , checkStatus
       , checkStatusPtr
       , fromFlagList
       , maybeWithCString
       , peekIntegral
       , peekIntegralArray
       , peekReal
       , peekRealArray
       , withIntegral
       , withIntegralArrayLen
       , withJoinedCString
       , withRealArrayLen

       , checkForeignPtr
       , getArray
       ) where

import Control.Exception ( throw, throwIO )
import Control.Monad     ( (>=>) )
import Data.List         ( intercalate )
import Foreign           ( FinalizerPtr, ForeignPtr, Ptr, Storable, (.|.), bit
                         , clearBit, maybeWith, newForeignPtr, nullPtr, peek
                         , peekArray, with, withArrayLen )
import Foreign.C         ( CInt, CString, withCString )

import Data.Grib.Raw.Exception
import Data.Grib.Raw.Types


checkStatus :: CInt -> IO ()
checkStatus 0      = return ()
checkStatus status = throwIO . GribException . toEnum . fromIntegral $ status

checkStatusPtr :: Ptr CInt -> IO ()
checkStatusPtr = peek >=> checkStatus

fromFlagList :: (Enum a, Integral b) => [a] -> b
fromFlagList = fromIntegral . foldr ((.|.) . fromEnum) zeroBits'
  -- Data.Bits.zeroBits is only available since base 4.7.0.0.
  where zeroBits' = clearBit (bit 0) 0

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString = maybeWith withCString

peekIntegral :: (Integral a, Storable a, Num b) => Ptr a -> IO b
peekIntegral = fmap fromIntegral . peek

peekIntegralArray :: (Integral a, Storable a, Num b) => Int -> Ptr a -> IO [b]
peekIntegralArray n = fmap (map fromIntegral) . peekArray n

peekReal :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekReal = fmap realToFrac . peek

peekRealArray :: (Real a, Storable a, Fractional b) => Int -> Ptr a -> IO [b]
peekRealArray n = fmap (map realToFrac) . peekArray n

withIntegral :: (Integral a, Num b, Storable b) => a -> (Ptr b -> IO c) -> IO c
withIntegral = with . fromIntegral

withIntegralArrayLen :: (Integral a, Num b, Storable b, Num c)
                     => [a] -> ((Ptr b, c) -> IO d) -> IO d
withIntegralArrayLen xs f =
  withArrayLen (map fromIntegral xs) $ \n xs' -> f (xs', fromIntegral n)

withJoinedCString :: [String] -> (CString -> IO a) -> IO a
withJoinedCString ss = withCString (intercalate "," ss)

withRealArrayLen :: (Real a, Fractional b, Storable b, Num c)
                 => [a] -> ((Ptr b, c) -> IO d) -> IO d
withRealArrayLen xs f =
  withArrayLen (map realToFrac xs) $ \n xs' -> f (xs', fromIntegral n)


checkForeignPtr :: (ForeignPtr a -> a) -> FinalizerPtr a -> Ptr a -> IO a
checkForeignPtr makeA finalizer p
  | p == nullPtr = throw NullPtrReturned
  | otherwise    = fmap makeA $ newForeignPtr finalizer p

getArray :: (Storable a, Integral b, Storable b)
         => (CString -> Ptr a -> Ptr b -> IO CInt)
         -> Key -> Ptr a -> Int -> IO [a]
getArray cCall key xs n =
  withCString key $ \key' -> with (fromIntegral n) $ \n' -> do
    cCall key' xs n' >>= checkStatus
    fmap fromIntegral (peek n') >>= flip peekArray xs
