{-# LANGUAGE FlexibleInstances #-}
module Test.Numeric_arbitrary where

import Test.QuickCheck
import System.Random

-- Import Data types we want arbitrary instances for.

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Numerics
import Mag

import Control.Applicative

instance (Arbitrary n, Random n, RealFloat n) => Arbitrary (UnitInterval n) where
    arbitrary = UnitInterval <$> choose (0.0, 1.0)

instance Arbitrary Vector1 where
  arbitrary = Vector1 <$> arbitrary

instance Arbitrary Vector2 where
  arbitrary = Vector2 <$> arbitrary <*> arbitrary

instance Arbitrary Vector3 where
  arbitrary = Vector3 <$> arbitrary <*> arbitrary <*> arbitrary

-- Is this default a good idea? Just about any type has values that
-- can't be normalized. E.g. 0, 0-vector, etc.
instance (Arbitrary n, Ord n, Num n, Mag n) =>
         Arbitrary (Normalized n) where
  arbitrary = (\ (NonZero a) -> normalize a) <$> arbitrary

-- !!!: The version above typechecks. Also see below.

arbitraryNonZero :: (Arbitrary a, Ord a, Num a) => Gen a
arbitraryNonZero = (\ (NonZero a) -> a) <$> arbitrary

-- !!!: It can't possibly typecheck with the signature "Gen a".
-- That'd mean you could generate a value of any time with it.
-- Here, we inherit a number of restrictions. You make use of
-- the call to "arbitrary" at type "NonZero a". The instance
-- defined in Test.QuickCheck.Modifiers has the form
--
-- instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonZero a)
--
-- This means you have to make the same restrictions for your
-- function, and also for the instance for "Normalized" above.
-- With this type signature, you can use "arbitraryNonZero",
-- but I'm not sure it's worth defining a separate function for
-- it, as the inlined version I've used above works just as well.

instance Arbitrary (Normalized Vector1) where
  arbitrary = normalize <$> Vector1 <$> arbitraryNonZero

instance Arbitrary (Normalized Vector2) where
  arbitrary = normalVector2 <$> (2*pi*) <$> arbitrary

instance Arbitrary (Normalized Vector3) where
  arbitrary = undefined
