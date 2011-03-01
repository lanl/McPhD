-- | Module for testing mesh types associated data type
module Mesh.Tests (tests) where

-- Testing libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

-- The library under test
import Mesh.SimpleCartesian

-- Its dependencies
import Data.Vector.V3
import Data.Ix


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

tests = [ testGroup "Index Tests"
          [ testCase "LEQ operator" testRange
          ],
          testGroup "Mesh Tests"
          [ testCase "Size Equality" testSize
          ]
        ]
