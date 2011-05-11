{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Space.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative


import Data.Vector.Class
import Space.Cartesian
import Space.Cartesian1D
import Space.Spherical1D
import NormalizedValues

import Test.Numeric_arbitrary ()

instance (Vector v, Arbitrary v,
          Arbitrary (Normalized v)) => Arbitrary (Cartesian v) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary

instance Arbitrary Cartesian1D where
  arbitrary = Cartesian1D <$> arbitrary <*> arbitrary

instance Arbitrary Spherical1D where
  arbitrary = Spherical1D <$> arbitrary <*> arbitrary
