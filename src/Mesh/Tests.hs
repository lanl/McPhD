import Test.HUnit
import Data.Vector.V3

import Mesh.Simple

simpleMesh = SimpleMesh (MeshSize 10 10 10) (Vector3 0.1 0.2 0.3)

test1 = TestCase (assertEqual "Size" (mesh_size simpleMesh) 1000)

testList = TestList [TestLabel "size" test1]

main = runTestTT testList
