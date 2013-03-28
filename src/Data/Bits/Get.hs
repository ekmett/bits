{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Bits.Get
  ( GetBits(..), getAligned, getBit
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Bytes.Get
import Data.Word

{-# ANN module "hlint: ignore Redundant $!" #-}

------------------------------------------------------------------------------
-- GetBits
------------------------------------------------------------------------------

newtype GetBits m a = GetBits
  { runGetBits :: forall r. (a -> Int -> Word8 -> m r) -> Int -> Word8 -> m r
  }

instance Functor (GetBits m) where
  fmap f (GetBits m) = GetBits $ \ k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative (GetBits m) where
  pure a = GetBits $ \k -> k a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (GetBits m) where
  return a = GetBits $ \ k -> k a
  {-# INLINE return #-}
  GetBits m >>= f = GetBits $ \ k -> m $ \a -> runGetBits (f a) k
  {-# INLINE (>>=) #-}

instance MonadTrans GetBits where
  lift m = GetBits $ \k i w -> do
    a <- m
    k a i w
  {-# INLINE lift #-}

-- | 'Get' something from byte-aligned storage, starting on the next byte
-- and flushing any left over bits.
getAligned :: MonadGet m => m a -> GetBits m a
getAligned m = GetBits $ \k _ _ -> m >>= \ a -> k a 0 0
{-# INLINE getAligned #-}

getBit :: MonadGet m => GetBits m Bool
getBit = GetBits $ \ k i b ->
  if i == 0
  then getWord8 >>= \b' -> ((k $! testBit b' 7) $! 7) $! shiftR b' 1
  else ((k $! testBit b 7) $! i - 1) $! shiftR b 1
{-# INLINE getBit #-}

