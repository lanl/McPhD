module Test.NormalizedValues_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- The library under test
import NormalizedValues

import Data.Vector.Class
import Approx
import Numerics
import Test.Numeric_arbitrary

-- | Property: sampleNormalVector3 returns normalized direction vectors.
prop_UnitLength :: (UnitInterval Double) -> (UnitInterval Double) -> Bool
prop_UnitLength a b = let v = sampleNormalVector3 a b in (vmag $ normalized_value v) ~== 1.0

tests = [ testGroup "Random Directions" [testProperty "unit length" prop_UnitLength] 
        ]

