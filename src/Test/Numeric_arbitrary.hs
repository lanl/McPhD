{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

{-- Arbitrary instances for various numeric data types for testing.
--}
module Test.Numeric_arbitrary where

import Test.QuickCheck
import System.Random

-- Import Data types we want arbitrary instances for.

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Numerics
import NormalizedValues

import Control.Applicative

instance (Arbitrary n, Random n, RealFloat n) =>
    Arbitrary (UnitInterval n) where
      arbitrary = UnitInterval <$> choose (0.0, 1.0)


{-- ???: Generating Overlapping instances with the Normalized
instances below. I'm surprised that the compiler doesn't choose
the more specific version in this case.

Example instances:

      instance (Arbitrary n, Ord n, Num n, NormalizedValues.Mag n) =>
               Arbitrary (NormalizedValues.Normalized n)

and

      instance Arbitrary (NormalizedValues.Normalized Vector2)

I can live without the default version.

--}

-- Default for Normalized Arbitrary instances uses normalize function
-- from class Mag and requires a NonZero argument
instance (Arbitrary n, Ord n, Num n, Mag n) =>
    Arbitrary (Normalized n) where
  arbitrary = (\ (NonZero a) -> normalize a) <$> arbitrary

-- TODO: Do you need the default for more than one type? It looks like the
-- vector instances are all special. Do they have to be?

instance Arbitrary Vector1 where
  arbitrary = Vector1 <$> arbitrary

instance Arbitrary Vector2 where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AzimuthAngle where
  arbitrary = sampleAzimuthAngle <$> arbitrary

instance Arbitrary ZenithAngle where
  arbitrary = sampleZenithAngle <$> arbitrary

instance Arbitrary Radius where
  arbitrary = sampleRadius <$> arbitrary

instance Arbitrary (Normalized Vector1) where
  arbitrary = normalize <$> Vector1 <$> (\ (NonZero a) -> a) <$> arbitrary

instance Arbitrary (Normalized Vector2) where
  arbitrary = normalVector2 <$> arbitrary

instance Arbitrary (Normalized Vector3) where
  arbitrary = normalVector3 <$> arbitrary <*> arbitrary
