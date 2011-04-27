{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module SpaceTime.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative


import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class
import SpaceTime.Cartesian
import SpaceTime.Spherical1D
import NormalizedValues

import Test.Numeric_arbitrary ()

instance (Vector v, Arbitrary v,
          Arbitrary (Normalized v)) => Arbitrary (Cartesian v) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary

instance Arbitrary Spherical1D where
  arbitrary = Spherical1D <$> arbitrary <*> arbitrary
