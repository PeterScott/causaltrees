{-# LANGUAGE TemplateHaskell #-}
import Data.CausalTree.Atom
import Data.CausalTree.Weft
import Data.CausalTree.SerDes
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Vector.Storable as SV

import Test.QuickCheck
import Test.QuickCheck.All

------------------------
-- QuickCheck properties
------------------------

-- SerDes.hs
prop_compressed_id n = runGet getC32 (runPut $ putC32 n) == n
prop_compressed_idP a b = runGet getP32 (runPut $ putP32 (a,b)) == (a,b)

-- Atom.hs
prop_encdec_atom id pred c = decode (encode atom) == atom
    where atom = TextAtom id pred c

prop_storable id pred c = SV.head vec == atom
    where atom = TextAtom id pred c
          vec  = SV.singleton atom

-- Weft.hs
prop_encdec_weftvec a b c d = decode (encode vv) == vv
    where vv = orderedListToWeft [(a, a+1), (b, b+1), (c, c+1), (d, d+1)] :: WeftVec

-- Top level
runQC = $(quickCheckAll)

main = runQC
