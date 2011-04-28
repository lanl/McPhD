-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

-- import Test.Sphere1D_test as Sphere1D
import Test.Mesh_test as Mesh
-- import Test.MC_test as MC
import Test.Geometry_Test as Geometry

all_tests = [testGroup "Mesh tests"     Mesh.tests
           , testGroup "Geometry Tests" Geometry.tests
            -- , testGroup "Sphere1D tests" Sphere1D.tests
            -- , testGroup "MC tests"       MC.tests
            ]

main :: IO ()
main = defaultMain all_tests
