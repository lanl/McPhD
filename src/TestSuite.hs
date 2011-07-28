-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

import Test.NormalizedValues_test as NormalizedValues
import Particle.Test.Particle_test as Particle
import Mesh.Test.Mesh_test as Mesh
import Coordinate.Test.Coordinate_test as Coordinate
import Test.Stream_test as Stream


all_tests = [ testGroup "NormalizedValue tests"    NormalizedValues.tests
            , testGroup "ParametricParticle tests" Particle.tests
            , testGroup "Mesh tests"               Mesh.tests
            , testGroup "Space tests"              Coordinate.tests
            , testGroup "Streaming tests"          Stream.tests
            ]

main :: IO ()
main = defaultMain all_tests
