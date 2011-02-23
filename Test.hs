import Data.CausalTree.Atom
import Data.CausalTree.Weft
import Data.CausalTree.SerDes
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Vector.Storable as SV

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

prop_storable id pred c = SV.head vec == atom
    where atom = TextAtom id pred c
          vec  = SV.singleton atom

-- Top level
runQC = sequence_ [ quickCheck prop_compressed_id
                  , quickCheck prop_compressed_idP
                  , quickCheck prop_encdec
                  , quickCheck prop_storable
                  ]

main = do
  putStrLn "Running QuickCheck tests..."
  runQC
  putStrLn "Testing serialization"
  let vv = orderedListToWeft [(1, 11), (3, 33), (4, 44), (10, 1000)] :: WeftVec
  print $ encode vv
