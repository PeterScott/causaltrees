{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Data.CausalTree.Weft (
              Weft ( emptyWeft
                   , getWeft
                   , setWeft
                   , extendWeft
                   , weftToList
                   , listToWeft
                   , weftToOrderedList
                   , orderedListToWeft
                   , mergeWefts
                   , underWeft
                   )
            , WeftMap
            , WeftVec
            , MemoDict
            , emptyMemoDict
            , addToMemoDict
            , pull
            ) where

import Data.CausalTree.Atom (AtomId, Yarn, Offset)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
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
    -- replaced. If you don't want this behavior, use 'extendWeft'.
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
    -- | Convert an unordered list of (yarn, offset) pairs into a 'Weft'.
    listToWeft :: [(Yarn, Offset)] -> a
    listToWeft = foldl' setWeft emptyWeft
    -- | Convert an ordered list of (yarn, offset) pairs into a 'Weft'.
    orderedListToWeft :: [(Yarn, Offset)] -> a
    orderedListToWeft = listToWeft
    -- | Merge two 'Weft's together into their union.
    mergeWefts :: a -> a -> a
    mergeWefts x y = foldl' setWeft x (weftToList y)
    -- | Is an 'AtomId' beneath the 'Weft'?
    underWeft :: AtomId -> a -> Bool
    underWeft (yarn, offset) w = offset <= (w `getWeft` yarn)

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
    setWeft = insertExtendVecWeft False
    extendWeft = insertExtendVecWeft True
    weftToList (WeftVec vec) = V.toList vec
    weftToOrderedList (WeftVec vec) = V.toList vec
    orderedListToWeft = WeftVec . V.fromList

-- Helper function for WeftVec instance. Inserts a (yarn, offset) pair into a
-- WeftVec, either extending any existing extry or replacing it.
insertExtendVecWeft :: Bool -> WeftVec -> (Yarn, Offset) -> WeftVec
insertExtendVecWeft extend (WeftVec vec) (yarn, offset) =
    case bisectRight vec yarn of
      0 -> WeftVec $ V.cons (yarn, offset) vec
      i -> let (y, o) = vec ! (i-1)
               winner = if extend then max o offset else offset
           in if y == yarn then
                  WeftVec $ vec `V.unsafeUpd` [((i-1), (yarn, winner))]
              else
                  WeftVec $ V.concat [ V.unsafeTake i vec
                                     , V.singleton (yarn, offset)
                                     , V.unsafeDrop i vec ]

instance Binary WeftVec where
    get = getWeftBinary
    put = putWeftBinary

----------------
-- Binary search
----------------

-- | Locate the insertion point for a key in a key-ordered @(key, value)@
-- vector, which lies to the right of any entry with an identical $f (key,
-- value)$. Requires that there be no two entries in the vector with the same
-- @f@ value.
bisectRight' :: Ord a => ((Yarn, Offset) -> a) -> Vector (Yarn, Offset) -> a -> Int
bisectRight' f vec key = bs 0 (V.length vec)
    where bs l r | l > r = l
                 | otherwise = case vec !? m of
                                 Nothing  -> V.length vec
                                 Just mid ->
                                     case compare (f mid) key of
                                       GT -> bs l (m-1)
                                       LT -> bs (m+1) r
                                       EQ -> m+1
                     where m = l + ((r - l) `div` 2)

bisectRight :: Vector (Yarn, Offset) -> Yarn -> Int
bisectRight = bisectRight' fst


--------------------
-- Memoization dicts
--------------------

-- | An id-to-weft memoization dict, for only those atoms whose predecessor is
-- in another yarn. This turns pulling into an O(1) operation. It consists of a
-- sorted vector of ids, and a parallel vector of their corresponding wefts.
--
-- 'WeftMap's are used here, because typically these wefts will share a lot in
-- common with each other.
data MemoDict = MemoDict !(Vector AtomId) !(BV.Vector WeftMap)
  deriving Show                 -- FIXME: remove this Show instance

emptyMemoDict :: MemoDict
emptyMemoDict = MemoDict V.empty BV.empty

-- | Add (id, weft) pair to the memoization map. Should be called whenever we
-- insert an atom, if and only if its predecessor is in another yarn. It's safe
-- to use pulling to determine the weft.
addToMemoDict :: MemoDict -> AtomId -> WeftMap -> MemoDict
addToMemoDict (MemoDict avec wvec) atom_id weft = MemoDict avec' wvec'
    where i = bisectRight' id avec atom_id
          avec' = V.concat [V.unsafeTake i avec, V.singleton atom_id, V.unsafeDrop i avec]
          wvec' = BV.concat [BV.unsafeTake i wvec, BV.singleton weft, BV.unsafeDrop i wvec]

-- | Return the awareness weft of a given id, which may optionally have a
-- predecessor given. This function will work if and only if 'addToMemoDict' has
-- been called for every atom whose predecessor is in another yarn. Takes O(1)
-- time.
pull :: MemoDict -> AtomId -> Maybe AtomId -> WeftMap
pull md@(MemoDict avec wvec) atom_id maybe_pred = 
    let w = case bisectRight' id avec atom_id of
              0 -> emptyWeft `setWeft` atom_id -- No weft found
              i -> let this_id   = avec `V.unsafeIndex`  (i - 1)
                       this_weft = wvec `BV.unsafeIndex` (i - 1)
                   in if this_id == atom_id then -- Id found. Return it.
                          this_weft
                      else if (fst this_id) /= (fst atom_id) then -- Different weave.
                               emptyWeft `setWeft` atom_id
                           else         -- Same weft, lower offset
                               this_weft `extendWeft` atom_id
    in case maybe_pred of
         Just pred -> mergeWefts w (pull md pred Nothing)
         Nothing   -> w

-- -- Some test values
-- ww = (emptyWeft :: WeftMap) `setWeft` (3, 33) `setWeft`
--       (1, 22) `setWeft` (1, 235) `extendWeft` (1, 1000)
-- md1 = addToMemoDict emptyMemoDict (3, 33) ww
-- md2 = addToMemoDict md1 (6, 69) (ww `extendWeft` (6, 69))
-- md3 = addToMemoDict md2 (5, 55) (ww `extendWeft` (77, 7777) `extendWeft` (5, 55))
