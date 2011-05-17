{-# LANGUAGE FlexibleContexts #-}


-- | Module for testing mesh types
module Mesh.Test.Mesh_test (tests) where

import Data.Functor
import Data.Maybe
import Data.Sequence as Seq
import Control.Applicative

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import Mesh.Classes
import Mesh.Spherical
import Mesh.Cartesian1D

-- Their dependencies
import Data.Vector.V2
import Data.Vector.V3
import Data.Ix
import Numerics
import RandomNumbers
import NormalizedValues
import Space.Cartesian1D
import Space.Classes

import Test.RandomNumbers_arbitrary


-- * Property functions which work on multiple mesh types

prop_SampleInMesh :: Mesh m => m -> Seed -> Bool
prop_SampleInMesh mesh seed =
    let (location, _) = uniform_sample mesh (makePureMT seed)
    in is_in_mesh mesh location


prop_FindIsInAgree :: Mesh m => m -> Seed -> Bool
prop_FindIsInAgree mesh seed =
  let rng = makePureMT seed
      (location, _) = uniform_sample mesh rng
      found_cell    = cell_find mesh location
  in case (is_in_cell mesh) <$> found_cell <*> Just location of
       Just True -> True
       _         -> False


-- * Functions for setting up assertions for multiple mesh types

assertDist :: (Mesh m, 
               Show (MeshFace m), Eq (MeshFace m), 
               Show (Distance (MeshSpace m)), Eq (Distance (MeshSpace m))) => 
              m -> MeshCell m 
              -> MeshSpace m 
              -> (Distance (MeshSpace m))
              -> Maybe (Distance (MeshSpace m), MeshFace m)
              -> Assertion
assertDist mesh cell location max_distance result =
  (cell_boundary mesh cell location max_distance) @?= result


-- | Property: distance is always > 0

-- | Property: distance is always < target.

-- | Property: Positive cos <-> Positive Face.

-- | Property: If target distance < boundary distance, always get
-- nothing.



-- * Spherical 1D Mesh tests

-- | A mesh to test with.
spherical_mesh :: SphericalMesh
spherical_mesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Vacuum

sph1DTestSize :: Assertion
sph1DTestSize = (size spherical_mesh) @?= 100

sph1DTestRadius :: Assertion
sph1DTestRadius = outer_radius spherical_mesh @?= Radius 100.0

-- | Function to create multiple distance assertions on spherical_mesh
sph1D_dist :: MeshCell SphericalMesh
           -> MeshSpace SphericalMesh
           -> Double
           -> Maybe (Double, MeshFace SphericalMesh)
           -> Assertion
sph1D_dist = assertDist spherical_mesh




-- * Cartesian1D mesh tests.

cartesian1D_mesh :: Cartesian1DMesh
cartesian1D_mesh = Cartesian1DMesh (Seq.fromList (fmap fromIntegral [0..100]))
                   Vacuum Reflection

cart1DTestSize :: Assertion
cart1DTestSize = (size cartesian1D_mesh) @?= 100

-- | Function for multiple distance assertions on cartesian1D_mesh
cart1D_distances :: MeshCell Cartesian1DMesh
                    -> MeshSpace Cartesian1DMesh
                    -> Double
                    -> Maybe (Double, MeshFace Cartesian1DMesh)
                    -> Assertion
cart1D_distances = assertDist cartesian1D_mesh

cart1D :: Double -> Vector2 -> Cartesian1D
cart1D x dir = Cartesian1D x $ unsafe_makeNormal dir

-- * Cartesian3D mesh tests







-- | Test Collection
tests = [ testGroup "Spherical Mesh Tests"
          [
            testCase "Size Equality" sph1DTestSize

          , testCase "Radius Equality" sph1DTestRadius

          , testCase "Limit too small"
            (sph1D_dist 0 (Vector2 0.5 0) 0.1 Nothing)

          , testCase "Radially outward to boundary"
            (sph1D_dist 0 (Vector2 0.5 0) 1.0 $ Just (0.5, Outward))

          , testCase "Radially inward to boundary"
            (sph1D_dist 1 (Vector2 (negate 1.5) 0) 1.0 $ Just (0.5, Inward))

          , testProperty "Locations sampled in mesh, are in mesh"
            (prop_SampleInMesh spherical_mesh)

          , testProperty "cell_find and is_in agree"
            (prop_FindIsInAgree spherical_mesh)
          ]
        , testGroup "Cartesian1D Mesh Tests"
          [
            testCase "Size Equality" cart1DTestSize

          , testProperty "Sampled locations are in mesh"
            (prop_SampleInMesh cartesian1D_mesh)

          , testProperty "cell_find and is_in agree"
            (prop_FindIsInAgree cartesian1D_mesh)

          , testCase "Limit too small"
            (cart1D_distances 0 (cart1D 0.5 $ Vector2 1.0 0.0) 0.1 Nothing)

          , testCase "Outward, straight to boundary"
            (cart1D_distances 1 (cart1D 0.5 $ Vector2 1.0 0.0) 1.0 $ Just (0.5, Mesh.Cartesian1D.Positive))

          , testCase "Inward, straight to boundary"
            (cart1D_distances 1 (cart1D 0.5 $ Vector2 (negate 1.0) 0.0) 1.0
            $ Just (0.5, Mesh.Cartesian1D.Negative))

          ]
        ]
