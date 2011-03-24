{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module SpaceTime.Test.Cartesian_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import SpaceTime.Cartesian

-- Arbitrary instances
import Test.Vector_arbitrary
import SpaceTime.Test.Cartesian_arbitrary


-- Its dependencies
import SpaceTime.Classes
import Approx
import Data.Functor
import Control.Applicative

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3


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
    ]
  ]
  