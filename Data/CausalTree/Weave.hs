{-# LANGUAGE BangPatterns #-}
module Data.CausalTree.Weave where

import Data.CausalTree.Atom
import Data.CausalTree.Weft
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector, (!?), (!))
import Foreign.Storable
import Data.Binary
import Data.List (nub, null)

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO (print x) `seq` x

--------------------------
-- High-level abstractions
--------------------------

class Weave w where
    -- | Return an empty 'Weave'
    emptyWeave      :: w
    -- | Weft covering all atoms in 'wvVec'
    weaveWeft       :: w -> WeftVec
    -- | Id-to-weft memoization dict
    weaveMemoDict   :: w -> MemoDict
    -- | Waiting set. A map of ids to patches blocking on them.
    weaveWaitingSet :: w -> M.HashMap AtomId Patch

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

chain1 = (False, V.fromList [taChar (1, 1) (0, 1) 'H', taChar (1, 2) (1, 1) 'e', 
                             taChar (1, 3) (1, 2) 'y'])
chain2 = (True,  V.fromList [taDeletor (1, 4) (5, 55), taDeletor (1, 5) (4, 44)])
chain3 = (False, V.fromList [taChar (1, 6) (7, 11) 'S', taChar (1, 7) (1, 6) 'u', 
                             taChar (1, 8) (1, 7) 'p'])
testPatch = Patch $ BV.fromList [chain1, chain2, chain3]
tp1 = Patch $ BV.singleton chain1

weave1 = emptyWeave { wvWeft = orderedListToWeft [(0, 2), (4, 57), (5, 100), (7, 15)] } -- ready
weave2 = emptyWeave { wvWeft = orderedListToWeft [(0, 2), (4, 41), (5, 100), (7, 10)] } -- block

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
patchValid (Patch chains) = fst $ BV.foldl' ((trace .) . checkChain) (True, first_atom_id) chains
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
          -- in sequence, starting w/ given id, from same user
          inSeq (yarn, off0) atoms = fst $ V.foldl' iter (True, off0) atoms
              where iter (False, o) _ = (False, o)
                    iter (True, offset) atom =
                        let (y, o) = atomId atom
                            good   = y == yarn && o == offset
                        in (good, o+1)
          -- in sequence, starting w/ given id, from same user, with all but
          -- first having the previous as its predecessor.
          inSeqPred (yarn, off0) atoms = f3 $ V.foldl' iter (True, off0, atomPred (V.head atoms)) atoms
              where iter (False, o, p) _ = (False, o, p)
                    iter (True, offset, pred) atom =
                        let (y, o) = atomId atom
                            good   = y == yarn && o == offset && atomPred atom == pred
                        in (good, o+1, atomId atom)
                    f3 (x, _, _) = x

--X checkChain :: (Bool, AtomId) -> Chain -> (Bool, AtomId)

-- FIXME: make patch validator

-----------------------
-- Simple vector weaves
-----------------------

-- A simple vector-based weave. This is a fairly specialized data type, using
-- 'TextAtom's in a 'Vector', covered by a 'WeftVec'.
data VectorWeave = 
    VectorWeave {
      -- | Unboxed 'Vector' of storable 'Atom's
      wvVec        :: Vector TextAtom,
      -- | Weft covering all atoms in 'wvVec'
      wvWeft       :: WeftVec,
      -- | Id-to-weft memoization dict
      wvMemoDict   :: MemoDict,
      -- | Waiting set. A map of ids to patches blocking on them.
      wvWaitingSet :: M.HashMap AtomId Patch
    }
  deriving Show

instance Weave VectorWeave where
    -- Empty weave: has start and end atoms, and weft covering them.
    emptyWeave = VectorWeave { wvVec = V.fromList [taStart, taEnd]
                             , wvWeft = orderedListToWeft [(0, 2)]
                             , wvMemoDict = emptyMemoDict
                             , wvWaitingSet = M.empty }
    weaveWeft       = wvWeft
    weaveMemoDict   = wvMemoDict
    weaveWaitingSet = wvWaitingSet
