{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.CausalTree.Atom where

import Data.Word
import Data.Char
import qualified Data.Vector.Unboxed as V
import Data.Binary
import Data.CausalTree.SerDes (getP32, putP32)

import Test.QuickCheck

--------------------------
-- High-level abstractions
--------------------------

-- | An atom id consists of a (yarn, offset) pair.
type AtomId = (Word32, Word32)

-- | An atom in a causal tree.
class Atom a where
    -- | Is the atom sticky?
    isSticky    :: a -> Bool
    -- | Get the atom id
    atomId      :: a -> AtomId
    -- | Get the predecessor id
    atomPred    :: a -> AtomId
    -- | Is this the start atom?
    isStartAtom :: a -> Bool
    -- | Is this the end atom?
    isEndAtom   :: a -> Bool


--------------------
-- Instance for text
--------------------

-- | A text atom contains either a Unicode character, a deletor, a
-- save-awareness atom, or a beginning/end atom. This is a strict, unpacked data
-- type. It always stores a char, which may have special values. E000 is start,
-- E001 is end, E002 is deletor, E003 is save-awareness. These are private use
-- characters and do not correspond to any Unicode characters.
data TextAtom = TextAtom !AtomId !AtomId !Char -- id pred char
   deriving (Show, Eq)

atomChar :: TextAtom -> Char
atomChar (TextAtom _ _ c) = c

-- | Is a 'TextAtom' a deletor atom?
isDeletor :: TextAtom -> Bool
isDeletor       = (=='\xE002') . atomChar
-- | Is a 'TextAtom' a save-awareness atom?
isSaveAwareness :: TextAtom -> Bool
isSaveAwareness = (=='\xE003') . atomChar

instance Atom TextAtom where
    isSticky (TextAtom _ _ c) = c >= '\xE000' && c <= '\xE003'
    atomId (TextAtom id _ _) = id
    atomPred (TextAtom _ pred _) = pred
    isStartAtom = (=='\xE000') . atomChar
    isEndAtom   = (=='\xE001') . atomChar

instance Binary TextAtom where
    put (TextAtom id pred char) = putP32 id >> putP32 pred >> put char
    get = do { id <- getP32; pred <- getP32; char <- get; return $ TextAtom id pred char }

-- | Construct a char 'TextAtom'
taChar :: AtomId -> AtomId -> Char -> TextAtom
taChar id pred c | c < '\xE000' || c > '\xE003' = TextAtom id pred c
                 | otherwise = error $ "taChar: reserved character " ++ (show c)
-- | Construct a deletor 'TextAtom'
taDeletor :: AtomId -> AtomId -> TextAtom
taDeletor id pred = TextAtom id pred '\xE002'
-- | Construct a save-awareness 'TextAtom'
taSaveAwareness :: AtomId -> AtomId -> TextAtom
taSaveAwareness id pred = TextAtom id pred '\xE003'
-- | Construct a start 'TextAtom'
taStart :: TextAtom
taStart = TextAtom (0, 1) (0, 1) '\xE000'
-- | Construct an end 'TextAtom'
taEnd :: TextAtom
taEnd = TextAtom (0, 2) (0, 1) '\xE001'


------------------------
-- QuickCheck properties
------------------------

prop_encdec id pred c = decode (encode atom) == atom
    where atom = TextAtom id pred c

test = mapM_ quickCheck [prop_encdec]
