-- | Module for testing mesh types
module Mesh.Test.Mesh_test (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

-- The libraries under test
import Mesh.SimpleCartesian
import Mesh.Spherical

-- Their dependencies
import Data.Vector.V3
import Data.Ix
import Numerics


-- * CellIndex tests

cellIndex :: CellIndex
cellIndex = CellIndex 10 10 10

testRange :: Assertion
testRange = inRange (CellIndex 0 0 0, CellIndex 20 20 20) cellIndex @? "InRange operator for CellIndex"


-- * Simple Mesh Tests

simpleMesh :: SimpleMesh
simpleMesh = SimpleMesh cellIndex (Vector3 0.1 0.2 0.3)

testSize :: Assertion
testSize = (meshSize simpleMesh) @?= 1000

-- * Spherical 1D Mesh tests
spherical_mesh :: SphericalMesh
spherical_mesh = SphericalMesh $ fmap Radius [1..100]

tests = [ testGroup "Index Tests"
          [ testCase "LEQ operator" testRange
          ],
          testGroup "Mesh Tests"
          [ testCase "Size Equality" testSize
          ]
        ]
