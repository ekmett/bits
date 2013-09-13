{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Bits.Coding
  ( Coding(..)
  -- * Get
  , getAligned, getBit, getBits, getBitsFrom
  -- * Put
  , putAligned, putUnaligned, putBit, putBits, putBitsFrom
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Bits
import Data.Bits.Extras
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Word

{-# ANN module "hlint: ignore Redundant $!" #-}

------------------------------------------------------------------------------
-- Coding
------------------------------------------------------------------------------

newtype Coding m a = Coding
  { runCoding :: forall r. (a -> Int -> Word8 -> m r) -> Int -> Word8 -> m r
  }

instance Functor (Coding m) where
  fmap f (Coding m) = Coding $ \ k -> m (k . f)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Coding m) where
  pure a = Coding $ \k -> k a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Coding m) where
  return a = Coding $ \ k -> k a
  {-# INLINE return #-}
  Coding m >>= f = Coding $ \ k -> m $ \a -> runCoding (f a) k
  {-# INLINE (>>=) #-}
  fail e = Coding $ \_ _ _ -> fail e
  {-# INLINE fail #-}

-- Binary.Get is strangely missing MonadPlus
instance (Monad m, Alternative m) => Alternative (Coding m) where
  empty = Coding $ \_ _ _ -> empty
  {-# INLINE empty #-}
  Coding m <|> Coding n = Coding $ \k i b -> do
    (a,i',b') <- m (\a i' b' -> pure (a,i',b')) i b <|> n (\a i' b' -> pure (a,i',b')) i b
    k a i' b'
  {-# INLINE (<|>) #-}

instance MonadPlus m => MonadPlus (Coding m) where
  mzero = Coding $ \_ _ _ -> mzero
  {-# INLINE mzero #-}
  mplus (Coding m) (Coding n) = Coding $ \k i b -> do
    (a,i',b') <- m (\a i' b' -> return (a,i',b')) i b `mplus` n (\a i' b' -> return (a,i',b')) i b
    k a i' b'
  {-# INLINE mplus #-}


instance MonadTrans Coding where
  lift m = Coding $ \k i w -> do
    a <- m
    k a i w
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (Coding m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance MonadReader e m => MonadReader e (Coding m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f (Coding m) = Coding $ \k i b -> do
    (a,i',b') <- local f $ m (\a i' b' -> return (a, i', b')) i b
    k a i' b'
  {-# INLINE local #-}

------------------------------------------------------------------------------
-- Get
------------------------------------------------------------------------------

-- | 'Get' something from byte-aligned storage, starting on the next byte
-- and discarding any left over bits in the buffer.
--
-- /NB:/ Using any operation from 'MonadGet' other than checking 'remaining' or
-- 'isEmpty' will implicitly perform this operation.
getAligned :: MonadGet m => m a -> Coding m a
getAligned m = Coding $ \k _ _ -> m >>= \ a -> k a 0 0
{-# INLINE getAligned #-}

-- | 'Get' a single bit, consuming an entire 'byte' if the bit buffer is empty
getBit :: MonadGet m => Coding m Bool
getBit = Coding $ \ k i b ->
  if i == 0
  then getWord8 >>= \b' -> ((k $! testBit b' 7) $! 7) $! unsafeShiftL b' 1
  else ((k $! testBit b 7) $! i - 1) $! unsafeShiftL b 1
{-# INLINE getBit #-}

getBits :: (MonadGet m, Bits b) => Int -> Int -> b -> Coding m b
getBits from to bits | from < to = return bits
                     | otherwise = do b <- getBit
                                      getBits (pred from) to $ assignBit bits from b
{-# INLINE getBits #-}

getBitsFrom :: (MonadGet m, Bits b) => Int -> b -> Coding m b
getBitsFrom from bits = getBits from 0 bits
{-# INLINE getBitsFrom #-}

instance MonadGet m => MonadGet (Coding m) where
  type Remaining (Coding m) = Remaining m
  type Bytes (Coding m) = Bytes m
  skip = getAligned . skip
  {-# INLINE skip #-}
  lookAhead (Coding m) = Coding $ \k i b -> lookAhead (m k i b)
  {-# INLINE lookAhead #-}
  lookAheadM (Coding m) = Coding $ \k i b -> lookAheadE (m (distribute k) i b) >>= factor
    where
      distribute k Nothing i' b'  = return $ Left $ k (Nothing) i' b'
      distribute k (Just a) i' b' = return $ Right $ k (Just a) i' b'
      factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Coding m) = Coding $ \k i b -> lookAheadE (m (distribute k) i b) >>= factor
    where
      distribute k (Left e) i' b'  = return $ Left $ k (Left e) i' b'
      distribute k (Right a) i' b' = return $ Right $ k (Right a) i' b'
      factor = either id id
  {-# INLINE lookAheadE #-}

  getBytes  = getAligned . getBytes
  {-# INLINE getBytes #-}
  remaining = lift remaining
  {-# INLINE remaining #-}
  isEmpty   = lift isEmpty
  {-# INLINE isEmpty #-}
  getWord8  = getAligned getWord8
  {-# INLINE getWord8 #-}
  getByteString = getAligned . getByteString
  {-# INLINE getByteString #-}
  getLazyByteString = getAligned . getLazyByteString
  {-# INLINE getLazyByteString #-}
  getWord16le = getAligned getWord16le
  {-# INLINE getWord16le #-}
  getWord32le = getAligned getWord32le
  {-# INLINE getWord32le #-}
  getWord64le = getAligned getWord64le
  {-# INLINE getWord64le #-}
  getWord16be = getAligned getWord16be
  {-# INLINE getWord16be #-}
  getWord32be = getAligned getWord32be
  {-# INLINE getWord32be #-}
  getWord64be = getAligned getWord64be
  {-# INLINE getWord64be #-}
  getWord16host = getAligned getWord16host
  {-# INLINE getWord16host #-}
  getWord32host = getAligned getWord32host
  {-# INLINE getWord32host #-}
  getWord64host = getAligned getWord64host
  {-# INLINE getWord64host #-}
  getWordhost = getAligned getWordhost
  {-# INLINE getWordhost #-}

------------------------------------------------------------------------------
-- Put
------------------------------------------------------------------------------

-- | Emit any remaining contents from the bit buffer.
--
-- Any use of the combinators from 'MonadPut' (including 'flush') will cause
-- this to happen.
putAligned :: MonadPut m => m a -> Coding m a
putAligned m = Coding $ \ k i b ->
 if i == 0
 then do
   a <- m
   k a 0 0
 else do
   putWord8 b
   a <- m
   k a 0 0

-- | 'Put' all the bits without a 'flush'
putUnaligned :: (MonadPut m, Bits b) => b -> Coding m ()
putUnaligned b = putBitsFrom (bitSize b) b
{-# INLINE putUnaligned #-}

-- | 'Put' a single bit, emitting an entire 'byte' if the bit buffer is full
putBit :: MonadPut m => Bool -> Coding m ()
putBit v = Coding $ \k i b ->
  if i == 7
  then do
    putWord8 (pushBit b i v)
    k () 0 0
  else (k () $! i + 1) $! pushBit b i v
  where
    pushBit w i False = clearBit w $ 7 - i
    pushBit w i True  = setBit   w $ 7 - i
{-# INLINE putBit #-}

-- TODO: Make putBits less stupid
-- | 'Put' a (closed) range of bits
putBits :: (MonadPut m, Bits b) => Int -> Int -> b -> Coding m ()
putBits from to b | from < to = return ()
                  | otherwise = putBit (b `testBit` from) >> putBits (pred from) to b
{-# INLINE putBits #-}

-- | @putBitsFrom from b = putBits from 0 b@
putBitsFrom :: (MonadPut m, Bits b) => Int -> b -> Coding m ()
putBitsFrom from b = putBits from 0 b
{-# INLINE putBitsFrom #-}

instance MonadPut m => MonadPut (Coding m) where
  putWord8 = putAligned . putWord8
  {-# INLINE putWord8 #-}
  putByteString = putAligned . putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = putAligned . putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = putAligned flush
  {-# INLINE flush #-}
  putWord16le = putAligned . putWord16le
  {-# INLINE putWord16le #-}
  putWord32le = putAligned . putWord32le
  {-# INLINE putWord32le #-}
  putWord64le = putAligned . putWord64le
  {-# INLINE putWord64le #-}
  putWord16be = putAligned . putWord16be
  {-# INLINE putWord16be #-}
  putWord32be = putAligned . putWord32be
  {-# INLINE putWord32be #-}
  putWord64be = putAligned . putWord64be
  {-# INLINE putWord64be #-}
  putWord16host = putAligned . putWord16host
  {-# INLINE putWord16host #-}
  putWord32host = putAligned . putWord32host
  {-# INLINE putWord32host #-}
  putWord64host = putAligned . putWord64host
  {-# INLINE putWord64host #-}
  putWordhost = putAligned . putWordhost
  {-# INLINE putWordhost #-}
