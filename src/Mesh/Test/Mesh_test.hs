-- | Module for testing mesh types
module Mesh.Test.Mesh_test (tests) where

import Data.Functor
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
import Data.Vector.V3
import Data.Ix
import Numerics
import RandomNumbers
import Mesh.Classes

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


-- * Spherical 1D Mesh tests

spherical_mesh :: SphericalMesh
spherical_mesh = SphericalMesh (Seq.fromList (fmap Radius [1..100])) Vacuum

sph1DTestSize :: Assertion
sph1DTestSize = (size spherical_mesh) @?= 100;

sph1DTestRadius :: Assertion
sph1DTestRadius = outer_radius spherical_mesh @?= Radius 100.0



-- * Cartesian1D mesh tests.

cartesian1D_mesh :: Cartesian1DMesh
cartesian1D_mesh = Cartesian1DMesh (Seq.fromList (fmap fromIntegral [0..100]))
                   Vacuum Reflection

cart1DTestSize :: Assertion
cart1DTestSize = (size cartesian1D_mesh) @?= 100;




-- * Cartesian3D mesh tests





tests = [ testGroup "Spherical Mesh Tests"
          [
            testCase "Size Equality" sph1DTestSize
          , testCase "Radius Equality" sph1DTestRadius

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

          ]
        ]
