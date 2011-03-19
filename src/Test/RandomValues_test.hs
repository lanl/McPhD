-- | A test module for RandomValues
module Test.RandomValues_test (tests) where


-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- The library under test
import RandomValues

-- Its dependencies
import SpaceTime.Space3DCartesian
import Data.Vector.Class
import Approx

-- | Property: randomDirection_compute returns normalized direction vectors.
prop_UnitLength :: Double -> Double -> Bool
prop_UnitLength a b = let v = randomDirection_compute a b in (vmag (dir v)) ~== 1.0



tests = [testGroup "Random Directions" [testProperty "unit length" prop_UnitLength] ]
