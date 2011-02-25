{-# LANGUAGE BangPatterns, ExistentialQuantification, RankNTypes #-}
module Data.CausalTree.Weave where

import Data.CausalTree.Atom
import Data.CausalTree.Weft
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as BV
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector, (!?), (!))
import Foreign.Storable
import Data.Binary
import Data.List (nub, null, foldl')
import qualified Control.Monad.State as State
import Control.Monad
import Data.STRef

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import Data.Monoid

import System.IO.Unsafe

--------------------------
-- High-level abstractions
--------------------------

class Weave w where
    -- | Return an empty 'Weave'
    emptyWeave      :: w
    -- | Weft covering all atoms in 'vwVec'
    weaveWeft       :: w -> WeftVec
    -- | Id-to-weft memoization dict
    weaveMemoDict   :: w -> MemoDict
    -- | Waiting set. A map of ids to patches blocking on them.
    weaveWaitingSet :: w -> M.HashMap AtomId Patch
    -- | Scour a weave, producing a lazy 'ByteString'.
--    scour           :: w -> ByteString

-- | A patch consists of a vector of chains. All of the chains must be inserted
-- atomically. Each chain must be either sticky or non-sticky (as indicated by
-- the Bool). A sticky chain must consist of atoms whose ids are in sequence,
-- from the same user, and they will not be applied until all their predecessors
-- are present in the weave. A non-sticky chain must consist of consecutive
-- same-yarn atoms, each of which is caused by the one before it, except for the
-- first one, which is anchored at some head-predecessor. A non-sticky chain
-- will not be inserted until its head-predecessor is present in the weave.
--
-- All chains in a patch must be from the same user. All offsets in the chains
-- must be in sequence, with no gaps. You must not have one non-sticky chain
-- have another chain as its predecessor, or the patch will never be
-- inserted. All these conditions must be verified by a patch validator. All
-- chains must be of non-zero length.
newtype Patch = Patch (BV.Vector (Bool, Vector TextAtom))
    deriving Show


-------------------
-- Patch operations
-------------------

-- chain1 = (False, V.fromList [taChar (1, 1) (0, 1) 'H', taChar (1, 2) (1, 1) 'e', 
--                              taChar (1, 3) (1, 2) 'y'])
-- chain2 = (True,  V.fromList [taDeletor (1, 4) (5, 55), taDeletor (1, 5) (4, 44)])
-- chain3 = (False, V.fromList [taChar (1, 6) (7, 11) 'S', taChar (1, 7) (1, 6) 'u', 
--                              taChar (1, 8) (1, 7) 'p'])
-- testPatch = Patch $ BV.fromList [chain1, chain2, chain3]
-- tp1 = Patch $ BV.singleton chain1

-- weave1 = emptyWeave { vwWeft = orderedListToWeft [(0, 2), (4, 57), (5, 100), (7, 15)] } -- ready
-- weave2 = emptyWeave { vwWeft = orderedListToWeft [(0, 2), (4, 41), (5, 100), (7, 10)] } -- block

-- | Is a 'Patch' ready to be applied to a 'Weave'?
patchReady :: Weave w => Patch -> w -> Bool
patchReady p w = null (patchBlockers p w)

-- | Get the list of 'AtomId's which are not present in a 'Weave' which must be
-- present for the 'Patch' to apply.
patchBlockers :: Weave w => Patch -> w -> [AtomId]
patchBlockers p w = filter (not . inWeave) (patchAnchors p)
    where inWeave a = a `underWeft` (weaveWeft w)

-- | Get the list of 'AtomId's which must be present for the 'Patch' to apply.
-- For a sticky chain, all predecessors must be present. For a non-sticky chain,
-- the head's predecessor must be present.
patchAnchors :: Patch -> [AtomId]
patchAnchors (Patch chains) = nub $ concat $ BV.toList $ BV.map chainAnchors chains
    where chainAnchors (True, atoms)  = map atomPred $ V.toList atoms
          chainAnchors (False, atoms) = [atomPred (V.head atoms)]
              
-- | Is a 'Patch' valid? All 'Patch'es coming from untrusted sources must be
-- checked with this function to ensure safety.
patchValid :: Patch -> Bool
patchValid (Patch chains) = fst $ BV.foldl' checkChain (True, first_atom_id) chains
    where checkChain :: (Bool, AtomId) -> (Bool, Vector TextAtom) -> (Bool, AtomId)
          checkChain (False, id) _ = (False, id)
          checkChain (True, start_id) (True, atoms)  = -- Sticky chain
              ((not $ V.null atoms) && inSeq start_id atoms && V.all outsideSpan atoms, lastPlusOne atoms)
          checkChain (True, start_id) (False, atoms) = -- Non-sticky chain
              ((not $ V.null atoms) && inSeqPred start_id atoms && outsideSpan (V.head atoms), lastPlusOne atoms)
          lastPlusOne atoms = if V.null atoms then (0, 0) else let (!y, !o) = atomId (V.last atoms) in (y, o+1)
          -- make sure atom's pred is outside this patch's span
          outsideSpan atom = let (_, o) = atomId atom in o >= low_offset && o <= high_offset
          first_atom_id = atomId $ V.head $ snd $ BV.head chains
          low_offset  = snd $ first_atom_id
          high_offset = snd $ atomId $ V.last $ snd $ BV.last chains
          -- in sequence, starting with given id, from same user
          inSeq (yarn, off0) atoms = fst $ V.foldl' iter (True, off0) atoms
              where iter (False, o) _ = (False, o)
                    iter (True, offset) atom =
                        let (y, o) = atomId atom
                            good   = y == yarn && o == offset
                        in (good, o+1)
          -- in sequence, starting with given id, from same user, with all but
          -- first having the previous as its predecessor.
          inSeqPred (yarn, off0) atoms = f3 $ V.foldl' iter (True, off0, atomPred (V.head atoms)) atoms
              where iter (False, o, p) _ = (False, o, p)
                    iter (True, offset, pred) atom =
                        let (y, o) = atomId atom
                            good   = y == yarn && o == offset && atomPred atom == pred
                        in (good, o+1, atomId atom)
                    f3 (x, _, _) = x


-----------------------
-- Simple vector weaves
-----------------------

-- A simple vector-based weave. This is a fairly specialized data type, using
-- 'TextAtom's in a 'Vector', covered by a 'WeftVec'.
data VectorWeave = 
    VectorWeave {
      -- | Unboxed 'Vector' of storable 'Atom's
      vwVec        :: Vector TextAtom,
      -- | Weft covering all atoms in 'wvVec'
      vwWeft       :: WeftVec,
      -- | Id-to-weft memoization dict
      vwMemoDict   :: MemoDict,
      -- | Waiting set. A map of ids to patches blocking on them.
      vwWaitingSet :: M.HashMap AtomId Patch
    }
  deriving Show

instance Weave VectorWeave where
    -- Empty weave: has start and end atoms, and weft covering them.
    emptyWeave = VectorWeave { vwVec = V.fromList [taStart, taEnd]
                             , vwWeft = orderedListToWeft [(0, 2)]
                             , vwMemoDict = emptyMemoDict
                             , vwWaitingSet = M.empty }
    weaveWeft       = vwWeft
    weaveMemoDict   = vwMemoDict
    weaveWaitingSet = vwWaitingSet

-- FIXME: IMPORTANT: when inserting save-awarenesses, insert after the end atom!

vwScour :: VectorWeave -> ByteString
vwScour = BB.toLazyByteString . vscour . vwVec
    where vscour vec = V.ifoldr scourPos mempty vec
              where scourPos i atom tl =
                        if isVisible atom && not (isDeletor (vec ! (i+1))) then
                            BB.fromChar (atomChar atom) `mappend` tl
                        else tl

-- $0101 T01a1 ea1a2 xa2b2 sa2a3 ^a3b1 ta3a4 #0102 *b2a5
cw = emptyWeave { vwVec = V.fromList v }
    where v = [ taStart 
              , taChar (1,1) (0,1) 'T'
              , taChar (1,2) (1,1) 'e'
              , taChar (2,2) (1,2) 'x'
              , taChar (1,3) (1,2) 's'
              , taDeletor (2,1) (1,3)
              , taChar (1,4) (1,3) 't'
              , taEnd
              , taSaveAwareness (1,5) (2,2)
              ]

-- $0101 T01a1 ea1a2 xa2b2 sa2a3 ^a3b1 ta3a4 #0102 *b2a5, with memo dict and weft.
cw_test = emptyWeave { vwVec = V.fromList v
                     , vwWeft = listToWeft [(0, 2), (1, 4)]
                     , vwMemoDict = memoDict }
    where v = [ taStart 
              , taChar (1,1) (0,1) 'T'
              , taChar (1,2) (1,1) 'e'
              , taChar (1,3) (1,2) 's'
              , taChar (1,4) (1,3) 't'
              , taEnd
              ]
          memoDict = addToMemoDict emptyMemoDict (1,1) (orderedListToWeft [(0, 2)])


------------------------
-- Weave transformations
------------------------

-- FIXME: add a proper class and stuff.

-- Things stored in the state:
-- * Current position in array :: Int
-- * List of non-sticky insertion chains :: [(Int, Vector TextAtom)]
-- * List of deletions :: [(Int, TextAtom)]
-- * List of save-awareness atoms :: [TextAtom]
-- All lists are backwards, so when copying, work backwards. :-|

data VwTransS = VwTransS { vwtIdx :: Int -- ^ Current index
                         , vwtWeave :: VectorWeave -- ^ Weave
                         , vwtInsChains :: [(Int, Vector TextAtom)] -- ^ non-sticky ins chains
                         , vwtDeletors :: [(Int, TextAtom)] -- ^ deletors and pred positions
                         , vwtDeletions :: [Int]            -- ^ indices to delete
                         , vwtSaveAwareness :: [TextAtom] -- ^ save-awareness atoms
                         } deriving Show
type VwTrans a = State.State VwTransS a

vwtInitialState weave = VwTransS { vwtIdx = 0, vwtWeave = weave
                                 , vwtInsChains = [], vwtDeletors = []
                                 , vwtDeletions = [], vwtSaveAwareness = []}

-- | Go to next cursor position. Will not go past weave length.
cursorNext :: VwTrans ()
cursorNext = State.modify trans
    where trans s = let i = min (vwtIdx s + 1) (maxIdx s) in s { vwtIdx = i }
          maxIdx = V.length . vwVec . vwtWeave

-- | Get the Atom at the current position in the cursor, if any.
cursorCurrent :: VwTrans (Maybe TextAtom)
cursorCurrent = State.get >>= \s -> return $ (vwVec (vwtWeave s)) !? (vwtIdx s)

-- | Get the Atom after the current position in the cursor, if any.
cursorPeek :: VwTrans (Maybe TextAtom)
cursorPeek = State.get >>= \s -> return $ (vwVec (vwtWeave s)) !? (vwtIdx s + 1)

-- | Is the cursor at the beginning of the weave?
cursorAtBeginning :: VwTrans Bool
cursorAtBeginning = State.get >>= (return . (==0) . vwtIdx)

-- | Is the cursor at the end of the weave?
cursorAtEnd :: VwTrans Bool
cursorAtEnd = do s <- State.get
                 let maxIdx = V.length (vwVec (vwtWeave s))
                 return $ vwtIdx s == maxIdx

-- | Insert a non-sticky chain at the current location. Updates awareness weft
-- and memoization dict.
cursorInsert :: Vector TextAtom -> VwTrans ()
cursorInsert chain = State.modify $ \s -> 
    s { vwtInsChains = (vwtIdx s, chain) : (vwtInsChains s) }

-- | Insert a deletion atom referring to the current location. Updates awareness
-- weft and memoization dict.
cursorDeleteWith :: TextAtom -> VwTrans ()
cursorDeleteWith delatom = State.modify $ \s -> 
    s { vwtDeletors = (vwtIdx s, delatom) : (vwtDeletors s) }

-- | Delete the current location.
cursorDelete :: VwTrans ()
cursorDelete = State.modify $ \s -> 
    s { vwtDeletions = (vwtIdx s) : (vwtDeletions s) }

-- | Tack a list of save-awareness atoms onto the weave. Updates awareness weft
-- and memoization dict.
cursorSaveAwareness :: [TextAtom] -> VwTrans ()
cursorSaveAwareness saAtoms = State.modify $ \s -> 
    s { vwtSaveAwareness = saAtoms ++ (vwtSaveAwareness s) }

-- Utility function: generate list [top..bottom], where top > bottom.
downTo :: Int -> Int -> [Int]
top `downTo` bottom | top == bottom = [top]
                    | otherwise = top : ((top-1) `downTo` bottom)

-- | Execute a cursor transformation on a weave.

-- FIXME: make this handle also the updating of memo dict and top weft. Once
-- vector is computed, use patchBlockers to find which waiting patches can now
-- be applied, and return those as well. Uncomment type decl.

--execCursor :: VectorWeave -> VwTrans a -> (VectorWeave, [Patch])
execCursor w trans = V.create createNewVector -- FIXME
    where t@(VwTransS idx _ insChains deletors deletions saAtoms) = State.execState trans initState
          initState = vwtInitialState w
          vw = vwVec w
          pop ref = modifySTRef ref tail
          -- Calculate size of resulting vector
          resultSize = (V.length vw) + (sum $ map (V.length . snd) insChains)
                       + (length deletors) + (length saAtoms) - (length deletions)
          -- Copy over all save-awareness atoms, and decrement j by that many.
          copy_awareness vec j =
            forM_ saAtoms $ \atom ->
                do jj <- readSTRef j
                   MV.write vec jj atom
                   writeSTRef j (jj-1)
          -- Process a deletor: copy it over, then copy the atom
          processDeletor vec i j deletor = do
            jj <- readSTRef j
            MV.write vec jj deletor
            MV.write vec (jj - 1) (vw ! i)
            writeSTRef j (jj - 2)
          -- Copy ins chain, then atom
          processInsChain vec i j chain = do
            forM_ ((V.length chain - 1) `downTo` 0) $ \k -> 
                do jj <- readSTRef j
                   MV.write vec jj (chain ! k)
                   writeSTRef j (jj-1)
            jj <- readSTRef j
            MV.write vec jj (vw ! i)
            writeSTRef j (jj - 1)
          -- Do the steps above, and return an initialized mutable vector
          createNewVector = do
            -- Allocate a mutable vector of the correct size
            vec <- MV.new resultSize
            j <- newSTRef $ resultSize - 1
            insChains_ <- newSTRef insChains
            deletors_ <- newSTRef deletors
            deletions_ <- newSTRef deletions
            copy_awareness vec j
            forM_ ((V.length vw - 1) `downTo` 0) $ \i ->
                do deletors'  <- readSTRef deletors_
                   insChains' <- readSTRef insChains_
                   deletions' <- readSTRef deletions_
                   (if deletors' /= [] && fst (head deletors') == i then
                        processDeletor vec i j (snd (head deletors')) >> pop deletors_
                    else
                        if insChains' /= [] && fst (head insChains') == i then
                            processInsChain vec i j (snd (head insChains')) >> pop insChains_
                        else 
                            if deletions' /= [] && head deletions' == i then
                                pop deletions_
                            else do
                              jj <- readSTRef j
                              MV.write vec jj (vw ! i)
                              writeSTRef j (jj-1))
            return vec
          
          

-- How to execute a cursor:
--
-- First, calculate the size of the resulting vector, in atoms. This will be:
-- Original size, PLUS sum of lengths of insertion chains, PLUS length of
-- deletors, PLUS length of save-awareness list, MINUS length of deletions
-- list. Allocate a mutable vector this size.
--
-- Next, starting at j = (len vec) - 1 and i = (len weave-vec) - 1 and going
-- backward down to 0 on both, we decide what to do for each i. First, copy over
-- all the save-awareness atoms, and decrement j by that many.

-- NEXT: If we see a deletion here, just i--. If we see a deletor here, copy it
-- to vec[j], copy wv[i] to vec[j-1], and j -= 2, i--. If we see an ins-chain
-- here, copy it in BACKWARDS, then copy over wv[i] to vec[j-len(chain)], and
-- decrement j by len(chain)+1, i--. Go until i < 0, then stop.

curPrint :: Show a => a -> VwTrans ()
curPrint x = unsafePerformIO (print x) `seq` return ()

-- Transform cw_test into cw
cwt_trans = do
  cursorNext
--  cursorDelete >> cursorNext -- Delete the 'T'
  cursorNext      -- cursor after 'e'
--  cursorCurrent >>= curPrint    -- print the 'e' atom
  cursorInsert $ V.singleton (taChar (2,2) (1,2) 'x')
  cursorNext                    -- cursor after 's'
--  cursorCurrent >>= curPrint    -- print the 'e' atom
  cursorDeleteWith $ taDeletor (2,1) (1,3) -- Delete 's'
  cursorSaveAwareness [taSaveAwareness (1,5) (2,2)]
