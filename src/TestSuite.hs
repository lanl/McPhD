-- | An executable to run all of the tests
import Test.Framework (defaultMain, testGroup)

-- Modules under test:

import Test.RandomStreaming_test as RandomStreaming
import Particle.Test.RandomParticle_test as RandomParticle
import Particle.Test.BasicParticle_test as BasicParticle
import Test.RandomValues_test as RandomValues
import Mesh.Tests as Mesh

all_tests = [ testGroup "BasicParticle tests"   BasicParticle.tests,
              testGroup "RandomParticle tests"  RandomParticle.tests,
              testGroup "RandomStreaming tests" RandomStreaming.tests,
              testGroup "RandomValue tests"     RandomValues.tests,
              testGroup "Mesh tests"            Mesh.tests]

main :: IO ()
main = defaultMain all_tests
