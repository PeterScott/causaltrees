module Data.CausalTree.Weft (
              Weft ( emptyWeft
                   , getWeft
                   , setWeft
                   , extendWeft
                   , weftToList
                   , listToWeft
                   , weftToOrderedList
                   , orderedListToWeft
                   )
            , WeftMap
            , WeftVec
            ) where

import Data.CausalTree.Atom (Yarn, Offset)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, (!?), (!))
import qualified Data.HashMap.Strict as M
import Data.List (foldl', sort)
import Data.Binary
import Data.CausalTree.SerDes (getP32, putP32, getC32, putC32)

--------------------------
-- High-level abstractions
--------------------------

-- | A 'Weft' is a mapping of yarns to maximum offsets. It can be implemented in
-- multiple ways, but all of these implementations use this same type class.
class Weft a where
    -- | Return an empty 'Weft'
    emptyWeft :: a
    -- | Get the top of a given yarn in a 'Weft', or 0 if the yarn is not found.
    getWeft :: a -> Yarn -> Offset
    -- | Set the offset of the given yarn to the value you specify. Note that if
    -- the value is smaller than the current offset, the current offset will be
    -- replaced. If you don't want this behavior, use extendWeft.
    setWeft :: a -> (Yarn, Offset) -> a
    -- | Set the offset of the given yarn to the value you specify or the
    -- existing value, whichever is larger.
    extendWeft :: a -> (Yarn, Offset) -> a
    extendWeft w (yarn, offset) = setWeft w (yarn, offset')
        where offset' = max offset (getWeft w yarn)
    -- | Return an unordered list of (yarn, offset) pairs.
    weftToList :: a -> [(Yarn, Offset)]
    -- | Return an ordered list of (yarn, offset) pairs.
    weftToOrderedList :: a -> [(Yarn, Offset)]
    weftToOrderedList = sort . weftToList
    -- | Convert an unordered list of (yarn, offset) pairs into a Weft.
    listToWeft :: [(Yarn, Offset)] -> a
    listToWeft = foldl' setWeft emptyWeft
    -- | Convert an ordered list of (yarn, offset) pairs into a Weft.
    orderedListToWeft :: [(Yarn, Offset)] -> a
    orderedListToWeft = listToWeft

-- Binary format: A 32-bit length, then a sequence of that many (yarn, offset)
-- pairs. All numbered are sent in SerDes compressed format.
putWeftBinary :: Weft a => a -> Put
putWeftBinary weft = putC32 (fromIntegral $ length lst) >> mapM_ putP32 lst
    where lst = weftToOrderedList weft

getWeftBinary :: Weft a => Get a
getWeftBinary = do len <- getC32
                   lst <- sequence $ take (fromIntegral len) $ repeat getP32
                   return $ orderedListToWeft lst

-----------------
-- Map-based weft
-----------------

-- | A hashmap-based implementation of a 'Weft'. This may share memory with
-- other, closely related wefts, and has O(lg n) time complexity for most
-- everything.
newtype WeftMap = WeftMap (M.HashMap Yarn Offset)
    deriving (Eq, Show)

instance Weft WeftMap where
    emptyWeft                          = WeftMap M.empty
    getWeft (WeftMap m) yarn           = case M.lookup yarn m of
                                           Just offset -> offset
                                           Nothing     -> 0
    setWeft (WeftMap m) (yarn, offset) = WeftMap $ M.insert yarn offset m
    weftToList (WeftMap m)             = M.toList m
    listToWeft                         = WeftMap . M.fromList

instance Binary WeftMap where
    get = getWeftBinary
    put = putWeftBinary

--------------------
-- Vector-based weft
--------------------

newtype WeftVec = WeftVec (Vector (Yarn, Offset))
    deriving (Eq, Show)

instance Weft WeftVec where
    emptyWeft = WeftVec V.empty
    getWeft (WeftVec vec) yarn =
        case bisectRight vec yarn of
          0 -> 0
          i -> let (y, offset) = vec ! (i-1)
               in if y == yarn then offset else 0
    setWeft (WeftVec vec) (yarn, offset) =
        case bisectRight vec yarn of
          0 -> WeftVec $ V.cons (yarn, offset) vec
          i -> let (y, _) = vec ! (i-1)
               in if y == yarn then
                      WeftVec $ vec `V.unsafeUpd` [((i-1), (yarn, offset))]
                  else
                      WeftVec $ V.concat [ V.unsafeTake i vec
                                         , V.singleton (yarn, offset)
                                         , V.unsafeDrop i vec ]
    weftToList (WeftVec vec) = V.toList vec
    weftToOrderedList (WeftVec vec) = V.toList vec
    orderedListToWeft = WeftVec . V.fromList

instance Binary WeftVec where
    get = getWeftBinary
    put = putWeftBinary

----------------
-- Binary search
----------------

-- | Locate the insertion point for a 'Yarn' in an ordered @(Yarn, Offset)@
-- vector, which lies to the right of any entry with an identical yarn. Requires
-- that there be no two entries in the vector with the same yarn.
bisectRight :: Vector (Yarn, Offset) -> Yarn -> Int
bisectRight vec yarn = bs 0 (V.length vec)
    where bs l r | l > r = l
                 | otherwise = case vec !? m of
                                 Nothing  -> V.length vec
                                 Just mid ->
                                     case compare (fst mid) yarn of
                                       GT -> bs l (m-1)
                                       LT -> bs (m+1) r
                                       EQ -> m+1
                     where m = l + ((r - l) `div` 2)
