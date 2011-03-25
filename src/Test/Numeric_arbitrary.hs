{-# LANGUAGE FlexibleInstances #-}
module Test.Numeric_arbitrary where

import Test.QuickCheck
import System.Random

-- Import Data types we want arbitrary instances for.

import Data.Vector.Class
import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Mag

import Control.Applicative

-- | A newtype for aribtrary RealFloat values in (0,1)
newtype Unit n = Unit n deriving (Show)

instance (Arbitrary n, Random n, RealFloat n) => Arbitrary (Unit n) where
    arbitrary = Unit <$> choose (0.0, 1.0)


instance Arbitrary Vector1 where
  arbitrary = Vector1 <$> arbitrary

instance Arbitrary Vector2 where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary

