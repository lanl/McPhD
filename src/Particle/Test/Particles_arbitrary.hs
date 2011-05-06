module Particle.Test.Particles_arbitrary where

import Control.Applicative
import Test.QuickCheck


import Space.Classes
import Particle.Test.Space3DCartesian_arbitrary
import Particle.ParametricParticle

instance (Space s, Arbitrary s) => Arbitrary (ParticleInSpace s) where
  arbitrary = createParticleInSpace
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary