{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Space.Test.Space_test where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import Space.Cartesian
import Space.Cartesian1D
import Space.Spherical1D

-- Its dependencies
import Space.Classes
import Approx
import Data.Functor
import NormalizedValues

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3

-- Arbitrary instances
import Test.Numeric_arbitrary
import Space.Test.Space_arbitrary

-- Property: Moving no distance leaves location unchanged.
prop_ZeroDistance :: (Num (Distance s), Space s, Approx s) => s -> Bool
prop_ZeroDistance location = (location ~== stream location 0)

-- Property: Any side of the triangle is less than the sum of the
-- other sides.
prop_TriangleInequality :: (Num (Distance s), Mag (Distance s),
                            Mag (Position s),
                            Space s, Approx s) => s -> Distance s -> Bool
prop_TriangleInequality location distance = let
    location' = location +-> distance
    mag_location  = magnitude $ position location
    mag_location' = magnitude $ position location'
    mag_distance  = magnitude distance
    in ((mag_location  + mag_location') >= mag_distance)  &&
       ((mag_location' + mag_distance)  >= mag_location ) &&
       ((mag_location  + mag_distance)  >= mag_location')
  where (~~>=) a b = (a>=b) || (a~==b)


tests =
  [
    testGroup "Zero distance"
    [
      testProperty
      "Zero distance -> Same location in 1D"
      (prop_ZeroDistance :: Cartesian1D -> Bool)
    , testProperty
      "Zero distance -> Same location in 2D"
      (prop_ZeroDistance :: Cartesian Vector2 -> Bool)
    , testProperty
      "Zero distance -> Same location in 3D"
      (prop_ZeroDistance :: Cartesian Vector3 -> Bool)
    , testProperty
      "Zero distance -> Same location in 1DSpherical"
      (prop_ZeroDistance :: Spherical1D -> Bool)
    ],
    testGroup "Triangle Inequality"
    [
      testProperty
      "Triangle inequality Cartesian 1D"
      (prop_TriangleInequality :: Cartesian1D -> Double -> Bool)
    , testProperty
      "Triangle inequality in Cartesian 2D"
      (prop_TriangleInequality :: Cartesian Vector2 -> Double  -> Bool)
    , testProperty
      "Triangle inequality in Cartesian 3D"
      (prop_TriangleInequality :: Cartesian Vector3 -> Double -> Bool)
    , testProperty
      "Triangle inequality in Spherical 1D"
      (prop_TriangleInequality :: Spherical1D -> Double -> Bool)
    ]
  ]
