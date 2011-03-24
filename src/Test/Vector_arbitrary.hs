module Test.Vector_arbitrary where

import Test.QuickCheck
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3

import Control.Applicative

instance Arbitrary Vector1 where
  arbitrary = Vector1 <$> arbitrary

instance Arbitrary Vector2 where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary


