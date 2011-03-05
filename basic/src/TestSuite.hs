-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

import Test.Sphere1D_test as Sphere1D
import Test.Mesh_test as Mesh

all_tests = [ testGroup "Sphere1D tests" Sphere1D.tests
            , testGroup "Mesh tests"     Mesh.tests
            ]

main :: IO ()
main = defaultMain all_tests
