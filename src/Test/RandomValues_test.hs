-- | A test module for RandomValues
module Test.RandomValues_test (tests) where


-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- The library under test
import RandomValues

-- Its dependencies
import Space3DCartesian
import Data.Vector.Class

approx :: Double -> Double -> Bool
approx a b = abs (a-b) < epsilon where
  epsilon = 1e-8

prop_UnitLength :: Double -> Double -> Bool
prop_UnitLength a b = let v = randomDirection_compute a b in approx (vmag (dir v)) 1.0



tests = [testGroup "Random Directions" [testProperty "unit length" prop_UnitLength] ]
        
