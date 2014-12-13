{-# LANGUAGE CPP, ForeignFunctionInterface, MagicHash, UnboxedTuples, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Calculate a number of fiddly bit operations using fast de Bruijn
-- multiplication tables.
--------------------------------------------------------------------
module Data.Bits.Extras
  ( Ranked(..)
  , log2
  , msb
  , w8
  , w16
  , w32
  , w64
  , assignBit
  , zeroBits
  , oneBits
  , srl
  ) where

import Data.Bits
import Data.Int
import Data.Word

#ifdef __GHCJS__
import Data.ByteString (ByteString, pack)
import Data.ByteString.Unsafe (unsafeIndex)
#else
import Foreign.Ptr
import Foreign.Storable
import GHC.Base
#endif

-- TODO: generalize to 64 bits, etc.
log2 :: Word32 -> Int
log2 !n0 = fromIntegral $ go (unsafeShiftR (n5 * 0x7C4ACDD) 27) where
  go :: Word32 -> Word8
  go !i = byteIdx debruijn_log32 (fromIntegral i)
  !n1 = n0 .|. unsafeShiftR n0 1
  !n2 = n1 .|. unsafeShiftR n1 2
  !n3 = n2 .|. unsafeShiftR n2 4
  !n4 = n3 .|. unsafeShiftR n3 8
  !n5 = n4 .|. unsafeShiftR n4 16
{-# INLINE log2 #-}

class (Num t, FiniteBits t) => Ranked t where
  -- | Calculate the least significant set bit using a debruijn multiplication table.
  -- /NB:/ The result of this function is undefined when given 0.
  lsb :: t -> Int
  lsb n = rank n - 1
  {-# INLINE lsb #-}

  -- | Calculate the number of trailing 0 bits.
  rank :: t -> Int
  rank 0 = 0
  rank n = lsb n + 1
  {-# INLINE rank #-}

  -- | Calculate the number of leading zeros.
  nlz :: t -> Int

instance Ranked Word64 where
  lsb n = fromIntegral $ go (unsafeShiftR ((n .&. (-n)) * 0x07EDD5E59A4E28C2) 58) where
    go :: Word64 -> Word8
    go i = byteIdx debruijn_lsb64 (fromIntegral i)
  {-# INLINE lsb #-}

  nlz x0 = popCount (complement x6) where
     x1 = x0 .|. unsafeShiftR x0 1
     x2 = x1 .|. unsafeShiftR x1 2
     x3 = x2 .|. unsafeShiftR x2 4
     x4 = x3 .|. unsafeShiftR x3 8
     x5 = x4 .|. unsafeShiftR x4 16
     x6 = x5 .|. unsafeShiftR x5 32
  {-# INLINE nlz #-}

instance Ranked Word32 where
  lsb n = fromIntegral $ go (unsafeShiftR ((n .&. (-n)) * 0x077CB531) 27) where
    go :: Word32 -> Word8
    go i = byteIdx debruijn_lsb32 (fromIntegral i)
  {-# INLINE lsb #-}

{-
  rank n = fromIntegral $ go (unsafeShiftR ((n .&. (-n)) * 0x4279976B) 26) where
    go :: Word32 -> Word8
    go i = byteIdx debruijn_rank32 (fromIntegral i)
  {-# INLINE rank #-}
-}

  nlz x0 = popCount (complement x5) where
     x1 = x0 .|. unsafeShiftR x0 1
     x2 = x1 .|. unsafeShiftR x1 2
     x3 = x2 .|. unsafeShiftR x2 4
     x4 = x3 .|. unsafeShiftR x3 8
     x5 = x4 .|. unsafeShiftR x4 16
  {-# INLINE nlz #-}


instance Ranked Word16 where
  lsb = lsb . w32
  {-# INLINE lsb #-}

  rank = rank . w32
  {-# INLINE rank #-}

  nlz x0 = popCount (complement x4) where
     x1 = x0 .|. unsafeShiftR x0 1
     x2 = x1 .|. unsafeShiftR x1 2
     x3 = x2 .|. unsafeShiftR x2 4
     x4 = x3 .|. unsafeShiftR x3 8
  {-# INLINE nlz #-}

instance Ranked Word8 where
  lsb = lsb . w32
  {-# INLINE lsb #-}

  rank = rank . w32
  {-# INLINE rank #-}

  nlz x0 = popCount (complement x3) where
     x1 = x0 .|. unsafeShiftR x0 1
     x2 = x1 .|. unsafeShiftR x1 2
     x3 = x2 .|. unsafeShiftR x2 4
  {-# INLINE nlz #-}

instance Ranked Int64 where
  lsb = lsb . w64
  {-# INLINE lsb #-}

  rank = rank . w64
  {-# INLINE rank #-}

  nlz = nlz . w64
  {-# INLINE nlz #-}

instance Ranked Int32 where
  lsb = lsb . w32
  {-# INLINE lsb #-}

  rank = rank . w32
  {-# INLINE rank #-}

  nlz = nlz . w32
  {-# INLINE nlz #-}

instance Ranked Int16 where
  lsb = lsb . w32
  {-# INLINE lsb #-}

  rank = rank . w32
  {-# INLINE rank #-}

  nlz = nlz . w16
  {-# INLINE nlz #-}

instance Ranked Int8 where
  lsb = lsb . w32
  {-# INLINE lsb #-}

  rank = rank . w32
  {-# INLINE rank #-}

  nlz = nlz . w8
  {-# INLINE nlz #-}

------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

w8 :: Integral a => a -> Word8
w8 = fromIntegral
{-# INLINE w8 #-}

w16 :: Integral a => a -> Word16
w16 = fromIntegral
{-# INLINE w16 #-}

w32 :: Integral a => a -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

w64 :: Integral a => a -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

-- | Calculate the most significant set bit.
msb :: Ranked t => t -> Int
msb n = finiteBitSize n - nlz n - 1
{-# INLINE msb #-}

assignBit :: Bits b => b -> Int -> Bool -> b
assignBit b n  True = b `setBit` n
assignBit b n False = b `clearBit` n
{-# INLINE assignBit #-}

oneBits :: Bits b => b
oneBits  = complement zeroBits

-- | Shift Right Logical (i.e., without sign extension)
--
-- /NB:/ When used on negative 'Integer's, hilarity may ensue.
srl :: Bits b => b -> Int -> b
srl b n = (b `shiftR` n) .&. rotateR (oneBits `shiftL` n) n
{-# INLINE srl #-}

------------------------------------------------------------------------------
-- de Bruijn Multiplication Tables
------------------------------------------------------------------------------

#ifdef __GHCJS__

byteIdx :: ByteString -> Int -> Word8
byteIdx = unsafeIndex
{-# INLINE byteIdx #-}

-- copied from cbits; is there a better way?

-- A 64 bit deBruijn multiplication table for calculating the the value of a single bit in a word64
debruijn_lsb64 :: ByteString
debruijn_lsb64 = pack [
 63,  0, 58,  1, 59, 47, 53,  2,
 60, 39, 48, 27, 54, 33, 42,  3,
 61, 51, 37, 40, 49, 18, 28, 20,
 55, 30, 34, 11, 43, 14, 22,  4,
 62, 57, 46, 52, 38, 26, 32, 41,
 50, 36, 17, 19, 29, 10, 13, 21,
 56, 45, 25, 31, 35, 16,  9, 12,
 44, 24, 15,  8, 23,  7,  6,  5
 ]
{-# NOINLINE debruijn_lsb64 #-}

-- A 32 bit deBruijn multiplication table for (n * 0x077CB531U) >> 27
debruijn_lsb32 :: ByteString
debruijn_lsb32 = pack [
   0,  1, 28,  2, 29, 14, 24, 3,
  30, 22, 20, 15, 25, 17,  4, 8,
  31, 27, 13, 23, 21, 19, 16, 7,
  26, 12, 18,  6, 11,  5, 10, 9
  ]
{-# NOINLINE debruijn_lsb32 #-}

-- -- Frigo's algorithm from http://stackoverflow.com/questions/7365562/de-bruijn-like-sequence-for-2n-1-how-is-it-constructed
-- debruijn_rank32 :: ByteString
-- debruijn_rank32 = pack [
--   32, -1,  2, -1,  3, -1, -1, -1,
--   -1,  4, -1, 17, 13, -1, -1,  7,
--    0, -1, -1,  5, -1, -1, 27, 18,
--   29, 14, 24, -1, -1, 20,  8, -1,
--   31,  1, -1, -1, -1, 16, 12,  6,
--   -1, -1, -1, 26, 28, 23, 19, -1,
--   30, -1, 15, 11, -1, 25, 22, -1,
--   -1, 10, -1, 21,  9, -1, -1, -1
--   ]
-- {-# NOINLINE debruijn_rank32 #-}

-- A 32 bit deBruijn multiplication table for calculating log_2 based on first rounding down to 1 less than a power of 2
debruijn_log32 :: ByteString
debruijn_log32 = pack [
   0,  9,  1, 10, 13, 21,  2, 29,
  11, 14, 16, 18, 22, 25,  3, 30,
   8, 12, 20, 28, 15, 17, 24,  7,
  19, 27, 23,  6, 26,  5,  4, 31
  ]
{-# NOINLINE debruijn_log32 #-}

#else

byteIdx :: Ptr Word8 -> Int -> Word8
byteIdx bs i = inlinePerformIO $ peekElemOff bs i
{-# INLINE byteIdx #-}

foreign import ccall "static &debruijn_lsb64"  debruijn_lsb64  :: Ptr Word8
foreign import ccall "static &debruijn_lsb32"  debruijn_lsb32  :: Ptr Word8
-- foreign import ccall "static &debruijn_rank32" debruijn_rank32 :: Ptr Word8
foreign import ccall "static &debruijn_log32"  debruijn_log32  :: Ptr Word8

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif

#endif
