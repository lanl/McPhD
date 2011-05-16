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
import Space.Spherical1D2

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
    in ((mag_location  + mag_location') ~>= mag_distance)  &&
       ((mag_location' + mag_distance)  ~>= mag_location ) &&
       ((mag_location  + mag_distance)  ~>= mag_location')
  where (~>=) a b = (a>=b) || (a~~==b)


-- | Spherical 1D tests

-- | Streaming from zero.
fromOrigin :: Assertion
fromOrigin = assertEqual "Stream 1.0 from origin"
             (Vector2 0.0 0.0 +-> 1.0) (Vector2 1.0 0.0)

toOrigin :: Assertion
toOrigin = assertEqual "Stream 1.0 to origin"
           (Vector2 (negate 1.0) 0.0 +-> 1.0) (Vector2 0.0 0.0)

throughOrigin :: Assertion
throughOrigin = assertEqual "Stream 2.0 through origin"
                (Vector2 (negate 1.0) 0.0 +-> 2.0) (Vector2 1.0 0.0)


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
        , testProperty
      "Zero distance -> Same location in 1DSpherical, 2nd formulation"
      (prop_ZeroDistance :: Spherical1D2 -> Bool)
    ]
  , testGroup "Triangle Inequality"
    [
      testProperty
      "Triangle inequality Cartesian 1D"
      (prop_TriangleInequality :: Cartesian Vector1 -> Double -> Bool)
    , testProperty
      "Triangle inequality in Cartesian 2D"
      (prop_TriangleInequality :: Cartesian Vector2 -> Double  -> Bool)
    , testProperty
      "Triangle inequality in Cartesian 3D"
      (prop_TriangleInequality :: Cartesian Vector3 -> Double -> Bool)
    , testProperty
      "Triangle inequality in Spherical 1D"
      (prop_TriangleInequality :: Spherical1D -> Double -> Bool)
    , testProperty
      "Triangle inequality in Spherical 1D, 2nd formulation"
      (prop_TriangleInequality :: Spherical1D2 -> Double -> Bool)
    ]
  , testGroup "Spherical 1D Streaming"
    [
      testCase "From the origin" fromOrigin
    , testCase "To the origin" toOrigin
    , testCase "Through the origin" throughOrigin
    ]
  ]
