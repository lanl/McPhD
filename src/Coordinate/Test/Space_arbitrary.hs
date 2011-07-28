{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Coordinate.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative


import Coordinate.Cartesian
import Coordinate.Cartesian1D

import Test.Numeric_arbitrary ()

instance Arbitrary Cartesian3D where
  arbitrary = Cartesian3D <$> arbitrary <*> arbitrary

instance Arbitrary Cartesian1D where
  arbitrary = Cartesian1D <$> arbitrary <*> arbitrary


-- TODO: Need instance for Spherical1D