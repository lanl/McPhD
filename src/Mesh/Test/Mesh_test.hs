-- | Module for testing mesh types
module Mesh.Test.Mesh_test (tests) where

import Data.Functor
import Control.Applicative

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The libraries under test
import Mesh.SimpleCartesian
import Mesh.Spherical

-- Their dependencies
import Data.Vector.V3
import Data.Ix
import Numerics
import RandomNumbers
import Mesh.Classes

import Test.RandomNumbers_arbitrary

-- * CellIndex tests

cellIndex :: CellIndex
cellIndex = CellIndex 10 10 10

testRange :: Assertion
testRange = inRange (CellIndex 0 0 0, CellIndex 20 20 20) cellIndex @? "InRange operator for CellIndex"


-- * Simple Mesh Tests

simpleMesh :: SimpleMesh
simpleMesh = SimpleMesh cellIndex (Vector3 0.1 0.2 0.3)

simpleTestSize :: Assertion
simpleTestSize = (meshSize simpleMesh) @?= 1000

-- * Spherical 1D Mesh tests
spherical_mesh :: SphericalMesh
spherical_mesh = SphericalMesh $ fmap Radius [1..100]

sph1DTestSize :: Assertion
sph1DTestSize = (size spherical_mesh) @?= 100;

sph1DTestRadius :: Assertion
sph1DTestRadius = outer_radius spherical_mesh @?= Radius 100.0


prop_SampleInMesh :: Seed -> Bool
prop_SampleInMesh seed =
    let (location, _) = uniform_sample spherical_mesh (makePureMT seed)
    in is_in_mesh spherical_mesh location
      


prop_FindIsInAgree :: Seed -> Bool
prop_FindIsInAgree seed =
  let rng = makePureMT seed
      (location, _) = uniform_sample spherical_mesh rng
      found_cell    = cell_find spherical_mesh location
  in case (is_in_cell spherical_mesh) <$> found_cell <*> Just location of
       Just True -> True
       _         -> False
      
      
      

tests = [ testGroup "Index Tests"
          [ 
           testCase "LEQ operator" testRange
          ],
          testGroup "Simple Mesh Tests"
          [ 
           testCase "Size Equality" simpleTestSize
          ],
          testGroup "Spherical Mesh Tests"
          [ 
           testCase "Size Equality" sph1DTestSize
          , testCase "Radius Equality" sph1DTestRadius
          , testProperty "Locations sampled in mesh, are in mesh" prop_SampleInMesh
          , testProperty "cell_find and is_in agree" prop_FindIsInAgree
          ]
        ]
