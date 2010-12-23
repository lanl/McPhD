module SimpleRandom_test where

-- Testing libraries
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit

-- The library under test
import SimpleRandom

-- Its dependencies
import Space
import Data.Vector.Class


tests = [
        testGroup "Random Directions" [
           testProperty "unit length" prop_UnitLength
           ]
        ]
        
approx :: Double -> Double -> Bool
approx a b = abs (a-b) < epsilon where
  epsilon = 1e-8

prop_UnitLength :: Double -> Double -> Bool
prop_UnitLength a b = let v = randomDirection_compute a b in approx (vmag (dir v)) 1.0

main :: IO ()
main = defaultMain tests 

