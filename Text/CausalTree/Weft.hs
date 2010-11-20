module Text.CausalTree.Weft (
              Weft
            , emptyWeft
            , getWeft
            , setWeft
            , extendWeft
            , weftToList

            , weft2ToWeft
            , weftToWeft2

            , WeftMap
            , WeftUArray
            ) where

import qualified Data.IntMap as IntMap
import Data.Word
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Array.IArray
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Text.Lazy as L
import Data.List (foldl')
import qualified Data.Text.Lazy.Builder as TLB

-- | A 'Weft' is a mapping of yarns to maximum offsets. It can be
-- implemented in multiple ways, but all of these implementations use
-- this same type class.
class Weft a where
    -- | Return an empty 'Weft'
    emptyWeft :: a
    -- | Get the top of a given yarn in a 'Weft', or 0 if the yarn is
    --   not found.
    getWeft :: a -> Char -> Word32
    -- | Set the offset of the given yarn to the value you
    --   specify. Note that if the value is smaller than the current
    --   offset, the current offset will be replaced. If you don't
    --   want this behavior, use extendWeft.
    setWeft :: a -> (Char, Word32) -> a
    -- | Set the offset of the given yarn to the value you specify or
    --   the existing value, whichever is larger.
    extendWeft :: a -> (Char, Word32) -> a
    extendWeft w (yarn, offset) = setWeft w (yarn, offset')
        where offset' = max offset (getWeft w yarn)
    -- | Return an ordered list of (yarn, offset) pairs.
    weftToList :: a -> [(Char, Word32)]
    -- | Convert an unordered list of (yarn, offset) pairs into a
    --   Weft.
    listToWeft :: [(Char, Word32)] -> a
    listToWeft = foldl' setWeft emptyWeft

-- | A map-based implementation of a 'Weft'. This may share memory
--   with other, closely related wefts, and has O(lg n) time
--   complexity for most everything.
newtype WeftMap = WeftMap (IntMap.IntMap Word32)
    deriving (Eq, Show)

instance Weft WeftMap where
    emptyWeft                          = WeftMap IntMap.empty
    getWeft (WeftMap m) yarn           = case IntMap.lookup (fromIntegral $ ord yarn) m of
                                           Just offset -> offset
                                           Nothing     -> 0
    setWeft (WeftMap m) (yarn, offset) = WeftMap $ IntMap.insert (fromIntegral $ ord yarn) offset m
    weftToList (WeftMap m)             = map (\(y, o) -> (chr y, o)) $ IntMap.assocs m

----------------------------------------------------------------------------------------------

-- | An unboxed array-based implementation of a 'Weft'. This is the
--   most compact way to represent a single weft, assuming that no
--   memory can be shared among multiple related wefts. It is
--   maintained in sorted order. Insertion takes O(n) time, lookup
--   takes O(lg n) time. The array consists of pairs of words, in
--   <yarn, offset> order, where the yarns words are char codes.
newtype WeftUArray = WeftUArray (UArray Int Word32)
    deriving (Eq, Show)

instance Weft WeftUArray where
    emptyWeft = WeftUArray $ array (0, -1) []
    -- Uses binary search, for O(lg n) access time.
    getWeft (WeftUArray a) yarn = case binSearch a yarnNum $ bounds' a of 
                                    Right i -> a ! (2*i + 1)
                                    Left  _ -> 0
        where yarnNum = fromIntegral $ ord yarn
    setWeft (WeftUArray a) (yarn, offset) = case binSearch a yarnNum $ bounds' a of 
                                              Right i -> WeftUArray $ a // [(2*i + 1, offset)]
                                              Left  j -> WeftUArray $ insArray a (yarnNum, offset) j
        where yarnNum = fromIntegral $ ord yarn
    weftToList (WeftUArray a) = chunkifyWeftArray $ elems a
        where chunkifyWeftArray (a : (b : tl)) = (chr $ fromIntegral a, b) : (chunkifyWeftArray tl)
              chunkifyWeftArray _              = []

bounds' a = (l, h `div` 2) where (l, h) = bounds a

-- | Binary search of a weft array 'a' for an element 'x', with bounds
--   (l, h). Returns an index in 'a', either a Right or a Left.
binSearch :: UArray Int Word32 -> Word32 -> (Int, Int) -> Either Int Int
binSearch a x (l, h) | l > h     = Left l
                     | otherwise = case compare (a ! (2 * m)) x of
                                     GT -> binSearch a x (l, (m-1))
                                     LT ->  binSearch a x ((m+1), h)
                                     EQ -> Right m
                           where m = l + ((h - l) `div` 2)

-- | In array 'a', insert a (yarnNum, offset) combo before index 'idx'.
insArray :: UArray Int Word32 -> (Word32, Word32) -> Int -> UArray Int Word32
insArray a (yarnNum, offset) idx = foo
       where (l, h) = bounds a
             foo = runSTUArray $ do
               arr <- newArray (l, h+2) 0 :: ST s (STUArray s Int Word32)
               mapM_ (\i -> writeArray arr i (a ! i)) [0..(idx*2 - 1)]
               writeArray arr (idx*2) yarnNum
               writeArray arr (idx*2 + 1) offset
               mapM_ (\i -> writeArray arr i (a ! (i-2))) [(idx*2+2)..(h+2)]
               return arr



----------------------------------------------------------------------------------------------

-- | Convert a weft2, such as "a5b3d1", to a 'Weft'. Not particularly
--   efficient, as it may reallocate a lot of arrays. The input weft2
--   need not be in sorted order.
weft2ToWeft :: (Weft a) => L.Text -> a
weft2ToWeft weft2 = listToWeft $ map tuplify $ L.chunksOf 2 weft2
    where tuplify txt = (L.head txt, fromIntegral $ (ord $ L.last txt) - (ord '0'))

-- | Convert a 'Weft' into a weft2, such as "a5b3d1". The resulting
--   weft2 will be in sorted order.
weftToWeft2 :: (Weft a) => a -> L.Text
weftToWeft2 = TLB.toLazyText . (foldl' mappend mempty) . (map toChunk) . weftToList
    where toChunk (yarn, offset) =
              (TLB.singleton yarn) `mappend` (TLB.singleton $ chr $ ((fromIntegral offset) + (ord '0')))

-- For debugging, some example data:
-- w1 = (weft2ToWeft $ L.pack "a5b3d1") :: WeftUArray
-- w2     :: WeftMap
-- w2_a5   = setWeft emptyWeft ('a', 5)
-- w2_a5b3 = setWeft w2_a5     ('b', 3)
-- w2      = setWeft w2_a5b3   ('d', 1)