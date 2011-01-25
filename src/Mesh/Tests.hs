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
import Data.Number.PartialOrd

import Data.Maybe


-- * CellIndex tests

cellIndex :: CellIndex
cellIndex = CellIndex 10 10 10

test1 :: Assertion
test1 = isJust (CellIndex 0 0 0 `lt` cellIndex) @? "LEQ operator for CellIndex"

test2 :: Assertion
test2 = isJust (cellIndex `lt` CellIndex 20 20 20) @? "LEQ operator for CellIndex"


-- * Simple Mesh Tests

simpleMesh :: SimpleMesh
simpleMesh = SimpleMesh cellIndex (Vector3 0.1 0.2 0.3)

test3 :: Assertion
test3 = (meshSize simpleMesh) @?= 1000

tests = [ testGroup "Index Tests"
	  [ testCase "LEQ operator"   test1,
	    testCase "LEQ operator 2" test2
	  ],
	  testGroup "Mesh Tests" [testCase "Size Equality" test3]
	]
