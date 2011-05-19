-- | A test module for Sphere1D
module Test.Source_Test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Test.Arbitraries

-- The library under test
import Source
import Physical
import PRNG

-- helpers
import Control.Applicative
import Debug.Trace

-- * energy sampling properties
-- | The power law rejector is bounded: specialized to a = 2.0 
-- (nondegenerate Fermi gas).
rejectorBndedA2 :: (Positive Double) -> Bool
rejectorBndedA2 (Positive x) = r >= 0.0 && r <= 1.0
  where r = rejector x 2.0

-- | More general case
rejectorBnded :: (Positive Double) -> Alpha -> Bool
rejectorBnded (Positive x) (Alpha a) = r >= 0.0 && r <= 1.0
  where r = rejector x a


tests = [testGroup "energy sampling" 
         [ testProperty "rejector: 0 <= r <= 1 (alpha = 2)" rejectorBndedA2
         , testProperty "rejector: 0 <= r <= 1 (alpha arb)" rejectorBnded
         ]
        ]
