-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

import Test.NormalizedValues_test as NormalizedValues

-- Sub-modules
import Particle.Test.BasicParticle_test as BasicParticle
import Particle.Test.ParametricParticle_test as ParametricParticle
import Mesh.Test.Mesh_test as Mesh
import SpaceTime.Test.Space_test as Space


all_tests = [ testGroup "BasicParticle tests"      BasicParticle.tests
            , testGroup "ParametricParticle tests" ParametricParticle.tests
            , testGroup "NormalizedValue tests"    NormalizedValues.tests
            , testGroup "Mesh tests"               Mesh.tests
            , testGroup "Space tests"              Space.tests
            ]

main :: IO ()
main = defaultMain all_tests
