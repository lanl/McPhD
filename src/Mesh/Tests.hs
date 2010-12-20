import Test.HUnit
import Data.Vector.V3

import Mesh.SimpleCartesian

cellIndex :: CellIndex
cellIndex = CellIndex 10 10 10

test1 :: Test
test1 = TestCase (assertBool "LEQ operator for CellIndex" (CellIndex 0 0 0 <=// cellIndex))

test2 :: Test
test2 = TestCase (assertBool "LEQ operator for CellIndex" (cellIndex <=// CellIndex 20 20 20))

indexTests :: Test
indexTests = TestList [TestLabel "test1" test1
                      , TestLabel "test2" test2]


-- Simple Mesh Tests

simpleMesh :: SimpleMesh
simpleMesh = SimpleMesh cellIndex (Vector3 0.1 0.2 0.3)

test3 :: Test
test3 = TestCase (assertEqual "Size" (mesh_size simpleMesh) 1000)


meshTests :: Test
meshTests = TestList [TestLabel "Size" test3]


main :: IO Counts
main = do
  _ <- runTestTT indexTests
  runTestTT meshTests
