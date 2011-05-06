-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

import Test.NormalizedValues_test as NormalizedValues

-- Sub-modules
import Particle.Test.ParametricParticle_test as ParametricParticle
import Mesh.Test.Mesh_test as Mesh
import Mesh.Test.Cartesian3D_test as CartesianMesh
import SpaceTime.Test.Space_test as Space
import Test.Stream_test as Stream


all_tests = [ testGroup "ParametricParticle tests" ParametricParticle.tests
            , testGroup "NormalizedValue tests"    NormalizedValues.tests
            , testGroup "Mesh tests"               Mesh.tests
            , testGroup "Cartesian Mesh tests"     CartesianMesh.tests
            , testGroup "Space tests"              Space.tests
            , testGroup "Streaming tests"          Stream.tests
            ]

main :: IO ()
main = defaultMain all_tests
