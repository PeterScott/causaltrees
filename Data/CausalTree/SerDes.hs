module Data.CausalTree.SerDes (putC32, putP32, getC32, getP32) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits

------------------------
-- Serialization helpers
------------------------

-- | Put a 'Word32', in a compressed format.  If the number is less than 254, it
-- will be put in a single byte. If the number will fit in 16 bits, it will be
-- serialized as three bytes. Otherwise, it will take five bytes.
putC32 :: Word32 -> Put
putC32 w | w < 254              = putWord8 (fromIntegral w)
         | w >= 54 && w < 65536 = putWord8 254 >> putWord16be (fromIntegral w)
         | otherwise            = putWord8 255 >> putWord32be w

-- | Get a 'Word32', in compressed format. The opposite of 'putC32'.
getC32 :: Get Word32
getC32 = do b <- getWord8
            case b of
              255 -> getWord32be
              254 -> getWord16be >>= (return . fromIntegral)
              _   -> return (fromIntegral b)

-- | Put a compressed pair.
putP32 :: (Word32, Word32) -> Put
putP32 (w1, w2) = putC32 w1 >> putC32 w2

-- | Get a compressed pair.
getP32 :: Get (Word32, Word32)
getP32 = do { w1 <- getC32; w2 <- getC32; return (w1, w2) }
