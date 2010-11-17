-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-- Finger Weaves: efficient, purely functional data structure for
-- representing a weave5c, based on Finger Trees.

module FingerWeave where

import Data.FingerTree
import Data.Monoid
import Data.Word
import Data.Char
import Data.Maybe
import qualified Data.Text as T

-- | Atoms consist of a character, a predecessor id, and an atom id.
data Atom5c = Atom5c { c :: Char, atom_pred :: (Char, Word32), atom_id :: (Char, Word32) }
            deriving (Show, Eq)

-- | Convert an 'Atom5c' to a 'Text' of length 5.
atomToText5 :: Atom5c -> T.Text
atomToText5 (Atom5c c (pc, po) (idc, ido)) = 
    T.pack [c, pc, offsetToChar po, idc, offsetToChar ido]

-- | Convert an 'Atom5c' to a 'Text' of length 3.
atomToText3 :: Atom5c -> T.Text
atomToText3 (Atom5c c _ (idc, ido)) = 
    T.pack [c, idc, offsetToChar ido]

-- | Convert a 'Text' of length 5 to an 'Atom5c'.
textToAtom :: T.Text -> Atom5c
textToAtom t 
    | (T.length t == 5) = Atom5c c (pc, charToOffset po) (idc, charToOffset ido)
    where (c,   t')     = fromJust $ T.uncons t
          (pc,  t'')    = fromJust $ T.uncons t'
          (po,  t''')   = fromJust $ T.uncons t''
          (idc, t'''')  = fromJust $ T.uncons t'''
          (ido, t''''') = fromJust $ T.uncons t''''

-- | Convert an offset 'Word32' to a character.
offsetToChar :: Word32 -> Char
offsetToChar offset = chr (fromIntegral offset + (ord '0'))

-- | Convert a character to an offset 'Word32'.
charToOffset :: Char -> Word32
charToOffset char = fromIntegral $ (ord char) - (ord '0')

----------------------------------------------------------------------------------------------

-- FIXME: add stringification here

--bar = '⌫'

data PartialScour = PS (Maybe (Char, Word32)) T.Text (Maybe (Char, Word32) -> T.Text)
                      | PSNull

instance Show PartialScour where
    show (PS d s nc) = "(PS " ++ (show d) ++ " " ++ (show s) ++ " [opaque])"

deleteMaybe :: Atom5c -> Maybe (Char, Word32) -> T.Text
deleteMaybe a Nothing = atomToText3 a
deleteMaybe a@(Atom5c _ _ (idc, ido)) (Just (pc, po)) = 
    if idc == pc && ido == po then
        T.empty
    else
        atomToText3 a

instance Monoid PartialScour where
    mempty = PSNull
    PSNull `mappend` x = x
    x `mappend` PSNull = x
    (PS d1 s1 nc1) `mappend` (PS d2 s2 nc2) =
        PS d1 (s1 `T.append` (nc1 d2) `T.append` s2) nc2

partiallyScour a@(Atom5c '⌫' (pc, po) (idc, ido)) = PS (Just (pc, po)) T.empty (\_ -> T.empty)
partiallyScour a = PS Nothing T.empty (deleteMaybe a)

-- | Turn a partial scour into a complete stringified text3.
completeScour :: PartialScour -> T.Text
completeScour PSNull      = T.empty
completeScour (PS _ s nc) = s `T.append` (nc Nothing)

hatom = (Atom5c { c = 'H', atom_pred = ('0', 1), atom_id = ('a', 1) })
hdatom = (Atom5c '⌫' ('a', 1) ('b', 1))


----------------------------------------------------------------------------------------------

data SizeAnd = SizeAnd Int PartialScour
            deriving Show

instance Monoid SizeAnd where
    mempty = SizeAnd 0 PSNull
    (SizeAnd x ps1) `mappend` (SizeAnd y ps2) = SizeAnd (x + y) (ps1 `mappend` ps2)

instance Measured SizeAnd Atom5c where
    measure x = SizeAnd 1 (partiallyScour x)

type SimpleSeq a = FingerTree SizeAnd a

foo = fromList [hatom, hdatom] :: SimpleSeq Atom5c

gtSizeAnd :: Int -> SizeAnd -> Bool
gtSizeAnd n (SizeAnd x _) = x > n

nth' :: SimpleSeq Atom5c -> Int -> Atom5c
nth' seq n = case viewl right of
               EmptyL -> error "index too large"
               x :< _ -> x
    where (_, right) = split (gtSizeAnd n) seq

insert' :: SimpleSeq Atom5c -> Int -> Atom5c -> SimpleSeq Atom5c
insert' seq n x = left >< (x <| right)
    where (left, right) = split (gtSizeAnd n) seq

append' :: SimpleSeq Atom5c -> Atom5c -> SimpleSeq Atom5c
append' = (|>)

prepend' :: Atom5c -> SimpleSeq Atom5c -> SimpleSeq Atom5c
prepend' = (<|)

-- | Scour a weave5c into a text3. Does not remove special symbols. FIXME: do this.
scour :: SimpleSeq Atom5c -> T.Text
scour seq = case measure seq of
                  SizeAnd _ x -> completeScour x

-- | Convert a text3 into a text1
stringify :: T.Text -> T.Text
stringify txt = case T.uncons txt of
                  Just (head, tail) -> T.cons head (drop2 tail)
                  Nothing           -> T.empty
    where drop2 = drop1     . T.tail
          drop1 = stringify . T.tail