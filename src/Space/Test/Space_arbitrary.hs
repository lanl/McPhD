{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Space.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative


import Space.Cartesian
import Space.Cartesian1D

import Test.Numeric_arbitrary ()

instance Arbitrary Cartesian3D where
  arbitrary = Cartesian3D <$> arbitrary <*> arbitrary

instance Arbitrary Cartesian1D where
  arbitrary = Cartesian1D <$> arbitrary <*> arbitrary


-- TODO: Need instance for Spherical1D