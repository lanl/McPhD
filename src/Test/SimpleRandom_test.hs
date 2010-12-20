module Main where

-- Testing libraries
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.Framework.Providers.HUnit

-- The library under test
import SimpleRandom

-- It's dependencies
import Space
import Data.Vector.Class


main :: IO ()
main = defaultMain tests 

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
