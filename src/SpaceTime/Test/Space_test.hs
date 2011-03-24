{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module SpaceTime.Test.Space_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import SpaceTime.Cartesian
import SpaceTime.Spherical1D

-- Its dependencies
import SpaceTime.Classes
import Approx
import Data.Functor

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3

-- Arbitrary instances
import Test.Vector_arbitrary
import SpaceTime.Test.Space_arbitrary

import Approx

-- Property: Moving no distance leaves location unchanged.
prop_ZeroDistance :: (Num (Distance s), Space s) => s -> Bool
prop_ZeroDistance location = (location == stream location 0)


tests =
  [
    testGroup "Stream Operations"
    [
      testProperty "Zero distance -> Same location in 1D" (prop_ZeroDistance :: Cartesian Vector1 -> Bool)
    , testProperty "Zero distance -> Same location in 2D" (prop_ZeroDistance :: Cartesian Vector2 -> Bool)
    , testProperty "Zero distance -> Same location in 3D" (prop_ZeroDistance :: Cartesian Vector3 -> Bool)
    , testProperty "Zero distance -> Same location in 1DSpherical" (prop_ZeroDistance :: Spherical1D -> Bool)
    ]
  ]
