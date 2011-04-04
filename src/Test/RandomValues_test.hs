-- | A test module for RandomValues
module Test.RandomValues_test (tests) where


-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- The library under test
import RandomValues

-- Its dependencies
import SpaceTime.Space3DCartesian
import Data.Vector.Class
import Approx
import Numerics
import Test.Numeric_arbitrary

-- | Property: sampleNormalVector3 returns normalized direction vectors.
prop_UnitLength :: Double -> Double -> Bool
prop_UnitLength a b = let v = sampleNormalVector3 a b in (vmag v) ~== 1.0

-- | Property: sampleExponential values are positive
prop_PositiveExponential :: (Positive Double) -> (UnitInterval Double) -> Bool
prop_PositiveExponential (Positive lambda) (UnitInterval xi) = let e = sampleExponential lambda xi in (e >= 0)

tests = [ testGroup "Random Directions" [testProperty "unit length" prop_UnitLength] 
        , testGroup "Random Exponentials" [testProperty "positive" prop_PositiveExponential]
        ]
