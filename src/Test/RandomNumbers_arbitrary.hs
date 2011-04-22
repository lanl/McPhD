module Test.RandomNumbers_arbitrary where

import Data.Functor
import Test.QuickCheck
import RandomNumbers

instance Arbitrary Seed where
  arbitrary = Seed <$> arbitrary
  
