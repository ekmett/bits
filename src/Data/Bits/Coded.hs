{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- License   :  BSD3
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Bits.Coded
  (
    Coded(..),
    Unary(..),
    runEncode,
    runDecode
  ) where

import Control.Monad
import Data.Bits.Coding
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Foldable

-- | Unaligned codes
class Coded c where
  encode     :: MonadPut m => c -> Coding m ()
  encodeMany :: (MonadPut m, Foldable t) => t c -> Coding m ()
  encodeMany = traverse_ encode
  decode     :: MonadGet m => Coding m c

-- | Unary-coded integers
--
-- >>> runPutL . runEncode $ encode (Unary 1) >> flush
-- "\128"
-- >>> runPutL . runEncode $ encode (Unary 7) >> flush
-- "\254"
newtype Unary n = Unary {
  unUnary :: n
} deriving (Eq, Ord, Read, Show, Num)

instance Integral n => Coded (Unary n) where
  encode (Unary n) = replicateM (fromIntegral n) (putBit True) >> putBit False
  decode = do b <- getBit
              case b of
                False -> return 0
                True  -> fmap (1+) decode

runEncode :: MonadPut m => Coding m () -> m ()
runEncode (Coding c) = c k 0 0
  where k _ _ _ = return ()

runDecode :: MonadGet m => Coding m a -> m a
runDecode (Coding c) = c k 0 0
  where k a _ _ = return a

