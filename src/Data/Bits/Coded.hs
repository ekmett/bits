{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- License   :  BSD3
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Bits.Coded
  (
    Coded(..)
    , Unary(..)
    , Elias(..)
    , Gamma
    , Delta
    , runEncode
    , runDecode
  ) where

import Data.Bits
import Data.Bits.Coding
import Data.Bits.Extras
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
} deriving (Eq, Ord, Read, Show, Num, Integral, Real, Enum)

ones :: Integer
ones = oneBits `unsafeShiftL` 1
instance Integral n => Coded (Unary n) where
  encode (Unary n) = putBitsFrom (fromIntegral n) ones
  decode = do b <- getBit
              if b then fmap (1+) decode else return 0

runEncode :: MonadPut m => Coding m () -> m ()
runEncode (Coding c) = c k 0 0
  where k _ _ _ = return ()

runDecode :: MonadGet m => Coding m a -> m a
runDecode (Coding c) = c k 0 0
  where k a _ _ = return a

-- | Representation for Elias 'Gamma' and 'Delta' codes.  A positive
-- integer @n@ is encoded by encoding the position of its most
-- significant bit, and then the binary representation of the rest of
-- the number.
newtype Elias c n = Elias {
  unElias :: n
} deriving (Eq, Ord, Read, Show, Num, Real, Integral, Enum)

instance (Coded c, Integral c, Ranked n) => Coded (Elias c n) where
  encode (Elias n) = do encode (fromIntegral m :: c)
                        putBitsFrom (pred m) n
    where m = msb n
  decode = do m :: c <- decode
              n <- getBitsFrom (pred $ fromIntegral m) (bit $ fromIntegral m)
              return $ Elias n

-- | Elias Gamma codes the position of the most significant in
-- 'Unary'.
type Gamma c n = Elias (Unary c)   n
-- | Elias Delta codes the position of the most significant bit in
-- Elias 'Gamma'.
type Delta c n = Elias (Gamma c n) n
