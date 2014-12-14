{-# LANGUAGE CPP, ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

module Data.Bits.Extras.Foreign
  ( debruijn_lsb64
  , debruijn_lsb32
  , debruijn_rank32
  , debruijn_log32
  , byteIdx
  ) where

import Foreign.Ptr
import Foreign.Storable
import GHC.Base
import Data.Word

------------------------------------------------------------------------------
-- de Bruijn Multiplication Tables
------------------------------------------------------------------------------

foreign import ccall "static &debruijn_lsb64"  debruijn_lsb64  :: Ptr Word8
foreign import ccall "static &debruijn_lsb32"  debruijn_lsb32  :: Ptr Word8
foreign import ccall "static &debruijn_rank32" debruijn_rank32 :: Ptr Word8
foreign import ccall "static &debruijn_log32"  debruijn_log32  :: Ptr Word8

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif

byteIdx :: Ptr Word8 -> Int -> Word8
byteIdx bs i = inlinePerformIO $ peekElemOff bs i
{-# INLINE byteIdx #-}
