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

-- | Atoms consist of a character, a predecessor id, and an atom id.
data Atom5c = Atom5c { c :: Char, atom_pred :: (Char, Word32), atom_id :: (Char, Word32) }
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

partiallyScour a@(Atom5c '⌫' (pc, po) (idc, ido)) = PS (Just (pc, po)) mempty (\_ -> mempty)
partiallyScour a = PS Nothing mempty (deleteMaybe a)

-- | Turn a partial scour into a complete stringified text3.
completeScour :: PartialScour -> L.Text
completeScour PSNull      = L.empty
completeScour (PS _ s nc) = TLB.toLazyText $ s `mappend` (nc Nothing)

hatom  = (Atom5c { c = 'H', atom_pred = ('0', 1), atom_id = ('a', 1) })
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

--foo = fromList [hatom, hdatom] :: FingerWeave

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
