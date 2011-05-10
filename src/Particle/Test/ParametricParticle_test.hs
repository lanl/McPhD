module Particle.Test.ParametricParticle_test where


-- Testing Libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

-- The library under test
import Particle.ParametricParticle

-- It's dependencies
import Particle.Test.Particles_arbitrary
import Approx
import Space.Classes
import Space.Spherical1D

-- | Property: Every particle is approximately equal to itself.
prop_Approx :: (Space s, Approx s) => ParticleInSpace s -> Bool
prop_Approx p = p ~== p

tests =
  [
    testGroup "Numeric Tests"
    [
      testProperty
      "Approximate identity"
      (prop_Approx :: ParticleInSpace Spherical1D -> Bool)
    ]
  ]
