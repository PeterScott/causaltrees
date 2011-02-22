import Data.CausalTree.Atom
import Data.CausalTree.Weft
import Data.CausalTree.SerDes
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Test.QuickCheck

------------------------
-- QuickCheck properties
------------------------

-- SerDes.hs
prop_compressed_id n = runGet getC32 (runPut $ putC32 n) == n
prop_compressed_idP a b = runGet getP32 (runPut $ putP32 (a,b)) == (a,b)

-- Atom.hs
prop_encdec id pred c = decode (encode atom) == atom
    where atom = TextAtom id pred c

-- Top level
main = sequence_ [ quickCheck prop_compressed_id
                 , quickCheck prop_compressed_idP
                 , quickCheck prop_encdec
                 ]
