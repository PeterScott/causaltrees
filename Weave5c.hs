-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-- Finger Weaves: efficient, purely functional data structure for
-- representing a weave5c, based on Finger Trees.

module Weave5c where

import Data.FingerTree
import Data.Monoid
import Data.Word
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as TLB
import Data.List (foldl')

import Weft

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
    where (c,   t')     = fromJust $ L.uncons t
          (pc,  t'')    = fromJust $ L.uncons t'
          (po,  t''')   = fromJust $ L.uncons t''
          (idc, t'''')  = fromJust $ L.uncons t'''
          (ido, t''''') = fromJust $ L.uncons t''''

-- | Convert an offset 'Word32' to a character.
offsetToChar :: Word32 -> Char
offsetToChar offset = chr (fromIntegral offset + (ord '0'))

-- | Convert a character to an offset 'Word32'.
charToOffset      :: Char -> Word32
charToOffset char = fromIntegral $ (ord char) - (ord '0')

----------------------------------------------------------------------------------------------

data PartialScour = PS (Maybe (Char, Word32)) TLB.Builder (Maybe (Char, Word32) -> TLB.Builder)
                  | PSNull

instance Show PartialScour where
    show (PS d s nc) = "(PS " ++ (show d) ++ " " ++ (show s) ++ " [opaque])"

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

partiallyScour a@(Atom5c '⌫' (pc, po) _) = PS (Just (pc, po)) mempty (\_ -> mempty)
partiallyScour a@(Atom5c '\2384' _ _)     = PS Nothing         mempty (\_ -> mempty)
partiallyScour a@(Atom5c '\1757' _ _)     = PS Nothing         mempty (\_ -> mempty)
partiallyScour a@(Atom5c '\8960' _ _)     = PS Nothing         mempty (\_ -> mempty)
partiallyScour a = PS Nothing mempty (deleteMaybe a)

-- | Turn a partial scour into a complete stringified text3.
completeScour :: PartialScour -> L.Text
completeScour PSNull      = L.empty
completeScour (PS _ s nc) = TLB.toLazyText $ s `mappend` (nc Nothing)

hatom  = (Atom5c 'H' ('0', 1) ('a', 1))
hdatom = (Atom5c '⌫' ('a', 1) ('b', 1))

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
sc_weave_txt = L.pack "\2384\&0101T01a1ea1a2xa2b2sa2a3\9003a3b1ta3a4\1757\&0102\8960b2a5"
sc_weave = weave5cToFingerWeave sc_weave_txt

-- | Convert a weave5c to a finger weave.
weave5cToFingerWeave :: L.Text -> FingerWeave
weave5cToFingerWeave = fromList . map text5ToAtom . L.chunksOf 5

-- | Convert a finger weave to a weave5c.
fingerWeaveToWeave5c :: FingerWeave -> L.Text
fingerWeaveToWeave5c seq = case measure seq of
                             FWMeasure _ _ x -> TLB.toLazyText x

gtFWMeasure :: Int -> FWMeasure -> Bool
gtFWMeasure n (FWMeasure x _ _) = x > n

nth' :: FingerWeave -> Int -> Atom5c
nth' seq n = case viewl right of
               EmptyL -> error "index too large"
               x :< _ -> x
    where (_, right) = split (gtFWMeasure n) seq

insert' :: FingerWeave -> Int -> Atom5c -> FingerWeave
insert' seq n x = left >< (x <| right)
    where (left, right) = split (gtFWMeasure n) seq

append' :: FingerWeave -> Atom5c -> FingerWeave
append' = (|>)

prepend' :: Atom5c -> FingerWeave -> FingerWeave
prepend' = (<|)

-- | Scour a weave5c into a text3. Does not remove special symbols. FIXME: do this.
scour :: FingerWeave -> L.Text
scour seq = case measure seq of
                  FWMeasure _ x _ -> completeScour x

-- | Convert a text3 into a text1
stringify :: L.Text -> L.Text
stringify txt   = case L.uncons txt of
                    Just (head, tail) -> L.cons head (drop2 tail)
                    Nothing           -> L.empty
    where drop2 = drop1     . L.tail
          drop1 = stringify . L.tail

----------------------------------------------------------------------------------------------

-- FIXME: make data structure mapping (yarn, offset) pairs to
-- wefts. Idea: use an IntMap from yarns to a finger tree structure
-- which supports fast append and find-lte-offset operations. This
-- shall be called a Quipu, made up of QuipuStrings.

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

-- emptyqs :: QuipuString WeftMap
-- emptyqs = QuipuString empty
-- fullqs  = foldl' quipuStringAdd emptyqs [(off1, w1), (off2, w2), (off3, w3), (off4, w4)]
-- fullqs' = quipuStringFromList [(off1, w1), (off2, w2), (off3, w3), (off4, w4)]
