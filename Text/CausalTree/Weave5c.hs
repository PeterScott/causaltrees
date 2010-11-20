-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-- Finger Weaves: efficient, purely functional data structure for
-- representing a weave5c, based on Finger Trees.

module Text.CausalTree.Weave5c {-( 
                 -- Tools for working with atoms
                 Atom5c,
               , atomToText5
               , atomToText3
               , text5ToAtom
               , offsetToChar
               , charToOffset
                 -- FIXME: replace these two with functions that
                 -- manipulate the Weave5c data type.
               , weave5cToFingerWeave -- FIXME: remove
               , fingerWeaveToWeave5c -- FIXME: remove
                 -- 
               , stringify
               , 
               )-} where

import Data.FingerTree
import Data.Monoid
import Data.Word
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as TLB
import Data.List (foldl')
import qualified Data.IntMap as IntMap

import Text.CausalTree.Weft

-- | Atoms consist of a character, a predecessor id, and an atom id.
data Atom5c = Atom5c Char (Char, Word32) (Char, Word32)
              deriving (Show, Eq)

-- | Convert an 'Atom5c' to a 'Text' of length 5.
atomToText5 :: Atom5c -> L.Text
atomToText5 (Atom5c c (pc, po) (idc, ido)) = 
    L.pack [c, pc, offsetToChar po, idc, offsetToChar ido]

-- | Convert an 'Atom5c' to a 'Text' of length 3.
atomToText3 :: Atom5c -> L.Text
atomToText3 (Atom5c c _ (idc, ido)) = 
    L.pack [c, idc, offsetToChar ido]

-- | Convert a 'Text' of length 5 to an 'Atom5c'.
text5ToAtom :: L.Text -> Atom5c
text5ToAtom t 
    | (L.length t == 5) = Atom5c c (pc, charToOffset po) (idc, charToOffset ido)
    | otherwise         = error "atoms must be of length 5"
    where (c,   t')     = fromJust $ L.uncons t
          (pc,  t'')    = fromJust $ L.uncons t'
          (po,  t''')   = fromJust $ L.uncons t''
          (idc, t'''')  = fromJust $ L.uncons t'''
          (ido, _)      = fromJust $ L.uncons t''''

-- | Convert an offset 'Word32' to a character.
offsetToChar :: Word32 -> Char
offsetToChar offset = chr (fromIntegral offset + (ord '0'))

-- | Convert a character to an offset 'Word32'.
charToOffset      :: Char -> Word32
charToOffset char = fromIntegral $ (ord char) - (ord '0')

----------------------------------------------------------------------------------------------

-- | The incremental scouring algorithm consists of building up
--   partially-scoured parser states. Each one has three parts:
-- 
--   * An initial deletor atom, if any.
--
--   * A string of atoms which are "settled". That is, they're not
--     going to be changed regardless of context.
-- 
--   * A function that takes a deletor atom (if any) from the next
--     block of the weave, and returns settled atoms.
-- 
--   The settled atoms take the form of a 'TLB.Builder', which allows
--   lazy concatenation and memory sharing of
--   substrings. 'PartialScour' is a monoid, and it's used as a
--   measure for a finger tree.
data PartialScour = PS (Maybe (Char, Word32)) TLB.Builder (Maybe (Char, Word32) -> TLB.Builder)
                  | PSNull

instance Show PartialScour where
    show (PS d s _) = "(PS " ++ (show d) ++ " " ++ (show s) ++ " [opaque])"
    show PSNull     = "PSNull"

deleteMaybe :: Atom5c -> Maybe (Char, Word32) -> TLB.Builder
deleteMaybe a Nothing = TLB.fromLazyText $ atomToText3 a
deleteMaybe a@(Atom5c _ _ (idc, ido)) (Just (pc, po)) = 
    if idc == pc && ido == po then
        mempty
    else
        TLB.fromLazyText $ atomToText3 a

instance Monoid PartialScour where
    mempty = PSNull
    PSNull `mappend` x = x
    x `mappend` PSNull = x
    (PS d1 s1 nc1) `mappend` (PS d2 s2 nc2) =
        PS d1 (s1 `mappend` (nc1 d2) `mappend` s2) nc2

partiallyScour                             :: Atom5c -> PartialScour
partiallyScour (Atom5c '\9003' (pc, po) _) = PS (Just (pc, po)) mempty (\_ -> mempty)
partiallyScour (Atom5c '\2384' _ _)        = PS Nothing         mempty (\_ -> mempty)
partiallyScour (Atom5c '\1757' _ _)        = PS Nothing         mempty (\_ -> mempty)
partiallyScour (Atom5c '\8960' _ _)        = PS Nothing         mempty (\_ -> mempty)
partiallyScour a                           = PS Nothing mempty (deleteMaybe a)

-- | Turn a partial scour into a complete stringified text3.
completeScour :: PartialScour -> L.Text
completeScour PSNull      = L.empty
completeScour (PS _ s nc) = TLB.toLazyText $ s `mappend` (nc Nothing)

--hatom  = (Atom5c 'H' ('0', 1) ('a', 1))
--hdatom = (Atom5c '\9003' ('a', 1) ('b', 1))

----------------------------------------------------------------------------------------------


data FWMeasure = FWMeasure Int PartialScour TLB.Builder
            deriving Show

instance Monoid FWMeasure where
    mempty = FWMeasure 0 PSNull mempty
    (FWMeasure x ps1 b1) `mappend` (FWMeasure y ps2 b2) =
        FWMeasure (x + y) (ps1 `mappend` ps2) (b1 `mappend` b2)

instance Measured FWMeasure Atom5c where
    measure x = FWMeasure 1 (partiallyScour x) (TLB.fromLazyText $ atomToText5 x)

type FingerWeave = FingerTree FWMeasure Atom5c

-- Example weave
--sc_weave_txt = L.pack "\2384\&0101T01a1ea1a2xa2b2sa2a3\9003a3b1ta3a4\1757\&0102\8960b2a5"
--sc_weave = weave5cToFingerWeave sc_weave_txt

-- | Convert a weave5c to a finger weave.
weave5cToFingerWeave :: L.Text -> FingerWeave
weave5cToFingerWeave = fromList . map text5ToAtom . L.chunksOf 5

-- | Convert a finger weave to a weave5c.
fingerWeaveToWeave5c :: FingerWeave -> L.Text
fingerWeaveToWeave5c fw = case measure fw of
                            FWMeasure _ _ x -> TLB.toLazyText x

gtFWMeasure :: Int -> FWMeasure -> Bool
gtFWMeasure n (FWMeasure x _ _) = x > n

-- FIXME: these finger weave manipulations should not be
-- exported. Instead, we should give functions for manipulating the
-- 'Weave5c' data type.

nth' :: FingerWeave -> Int -> Atom5c
nth' fw n = case viewl right of
              EmptyL -> error "index too large"
              x :< _ -> x
    where (_, right) = split (gtFWMeasure n) fw

insert' :: FingerWeave -> Int -> Atom5c -> FingerWeave
insert' fw n x = left >< (x <| right)
    where (left, right) = split (gtFWMeasure n) fw

append' :: FingerWeave -> Atom5c -> FingerWeave
append' = (|>)

prepend' :: Atom5c -> FingerWeave -> FingerWeave
prepend' = (<|)

-- | Scour a weave5c into a text3. Does not remove special symbols. FIXME: do this.
scour' :: FingerWeave -> L.Text
scour' fw = case measure fw of
              FWMeasure _ x _ -> completeScour x

-- | Convert a text3 into a text1
stringify :: L.Text -> L.Text
stringify txt   = case L.uncons txt of
                    Just (hd, tl) -> L.cons hd (drop2 tl)
                    Nothing       -> L.empty
    where drop2 = drop1     . L.tail
          drop1 = stringify . L.tail

----------------------------------------------------------------------------------------------

-- | A 'QuipuString' is a data structure which stores (offset, weft)
--   pairs for a single yarn. It is meant to support two operations
--   efficiently:
-- 
--   * Add a new weft with a higher offset than any yet stored. O(1).
-- 
--   * Find the highest-offset weft with an offset less than or equal
--     to a given offset. O(lg n).
-- 
--   In order to efficiently support these operations, we use a finger
--   tree data structure. It measures the offsets, and the monoid
--   operation returns the maximum of the offsets it compares.
newtype (Weft a) => QuipuString a = QuipuString (FingerTree MaxOffsetMeasure (Word32, a))
    deriving (Eq, Show)

-- Max offset measure: computes the maximum offset in a given range.
newtype MaxOffsetMeasure = MaxOffsetMeasure Word32

instance Monoid MaxOffsetMeasure where
    mempty = MaxOffsetMeasure 0
    (MaxOffsetMeasure x) `mappend` (MaxOffsetMeasure y) = MaxOffsetMeasure $ max x y

instance (Weft a) => Measured MaxOffsetMeasure (Word32, a) where
    measure = MaxOffsetMeasure . fst

-------------------------
-- The QuipuString API --
-------------------------

-- | Add an (offset, weft) pair to the end of a 'QuipuString'. The
--   offset must be higher than any previous offset in the
--   'QuipuString', or the result is undefined and will probably be
--   absolutely disastrous. Takes O(1) time.
quipuStringAdd :: (Weft a) => QuipuString a -> (Word32, a) -> QuipuString a
quipuStringAdd (QuipuString ft) pair = QuipuString $ ft |> pair

-- | Find the highest-offset weft with an offset less than or equal to
--   the given offset. If there is none, which should never happen, it
--   will return the weft with the lowest offset in the
--   'QuipuString'. If the 'QuipuString' is empty, which also should
--   never happen, it will return an empty weft located at offset 0,
--   which has no real semantic meaning but should be relatively
--   safe. Takes O(lg n) time.
quipuStringFindLte :: (Weft a) => QuipuString a -> Word32 -> (Word32, a)
quipuStringFindLte (QuipuString ft) offset =
    case viewr left of
      -- This case should never happen, but we deal with it anyway.
      EmptyR -> case viewl ft of
                  EmptyL -> (0, emptyWeft) -- This should also never happen.
                  x :< _ -> x
      _ :> x -> x
    where (left, _) = split (\(MaxOffsetMeasure o) -> o > offset) ft

-- | Create a 'QuipuString' from a list of (offset, weft) pairs,
--   ordered by offset. Takes O(n) time.
quipuStringFromList :: (Weft a) => [(Word32, a)] -> QuipuString a
quipuStringFromList =  QuipuString . fromList

-- -- Some data for debugging
-- w1 = weft2ToWeft $ L.pack "a2b4" :: WeftMap
-- w2 = weft2ToWeft $ L.pack "a3b4" :: WeftMap
-- w3 = weft2ToWeft $ L.pack "a3b5" :: WeftMap
-- w4 = weft2ToWeft $ L.pack "a6b8" :: WeftMap
-- off1 = 2
-- off2 = 7
-- off3 = 8
-- off4 = 20
-- 
-- emptyqs :: QuipuString WeftMap
-- emptyqs = QuipuString empty
-- fullqs  = foldl' quipuStringAdd emptyqs [(off1, w1), (off2, w2), (off3, w3), (off4, w4)]
-- fullqs' = quipuStringFromList [(off1, w1), (off2, w2), (off3, w3), (off4, w4)]
-- partqs  = quipuStringFromList [(off1, w1), (off3, w3)]

-- | A map from (yarn, offset) pairs to the highest-offset weft in
--   that yarn with an offset less than the given offset. These
--   lookups take O(lg n) time. It's implemented internally as an
--   'IntMap' of 'QuipuString' finger-tree-based data structures.
newtype (Weft a) => Quipu a = Quipu (IntMap.IntMap (QuipuString a))
    deriving (Eq, Show)

yarnToInt :: Char -> Int
yarnToInt = fromIntegral . ord

-- | An empty 'Quipu'.
quipuEmpty :: (Weft a) => Quipu a
quipuEmpty = Quipu IntMap.empty

-- | Look up an entry in a 'Quipu' corresponding to the given
--   offset. If no such entry is found, which should never happen, it
--   will return an empty weft with offset 0. Takes O(lg n) time.
quipuLookup :: (Weft a) => Quipu a -> (Char, Word32) -> (Word32, a)
quipuLookup (Quipu m) (yarn, offset) =
    case IntMap.lookup (yarnToInt yarn) m of
      Just qs -> quipuStringFindLte qs offset
      Nothing -> (0, emptyWeft)

-- | Add a 'QuipuString' to a 'Quipu' with a given yarn. Any existing
--   'QuipuString' with that yarn will be replaced. This is not likely
--   to be very useful to end users, but it might be, so we're making
--   it part of the API anyway.
quipuAddString :: (Weft a) => Quipu a -> Char -> QuipuString a -> Quipu a
quipuAddString (Quipu m) yarn qs = Quipu $ IntMap.insert (yarnToInt yarn) qs m

-- | Add a (yarn, offset) to 'Weft' mapping to the given 'Quipu', and
--   return the new 'Quipu' produced by this. The offset must be
--   larger than any other offset in the given yarn, or quiet and
--   hard-to-debug calamity will result. Takes O(1) time. If you
--   attempt to add to an empty yarn, the offset must be 1, or there
--   will be an error.
quipuAdd :: (Weft a) => Quipu a -> (Char, Word32) -> a -> Quipu a
quipuAdd (Quipu m) (yarn, offset) weft =
    case IntMap.lookup yarnInt m of
      Just qs -> Quipu $ IntMap.insert yarnInt (qs `quipuStringAdd` (offset, weft)) m
      Nothing -> if offset == 1 then
                     Quipu $ IntMap.insert yarnInt (QuipuString $ fromList [(offset, weft)]) m
                 else 
                     error "Offset for first Quipu entry in a yarn must be 1"
    where yarnInt = yarnToInt yarn

-- -- Some data for debugging
-- qpu1 :: Quipu WeftMap
-- qpu1 = quipuAddString (quipuAddString quipuEmpty 'g' fullqs) 's' partqs

----------------------------------------------------------------------------------------------

-- | A 'Weave5c' contains a 'FingerWeave' which has the atom data
--   itself, a 'Quipu' to store awareness wefts for each atom whose
--   predecessor is in a different yarn, and a 'WeftUArray' to store
--   the rightmost weft of the weave.
data Weave5c = Weave5c FingerWeave (Quipu WeftMap) WeftUArray
               deriving Show

blank_weave :: Weave5c
blank_weave = Weave5c (weave5cToFingerWeave (L.pack "\2384\&0101\1757\&0102")) quipuEmpty emptyWeft

scour :: Weave5c -> L.Text
scour (Weave5c fw _ _) = scour' fw

