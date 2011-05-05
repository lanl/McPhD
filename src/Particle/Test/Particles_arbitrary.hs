module Particle.Test.Particles_arbitrary where

import Control.Applicative
import Test.QuickCheck


import SpaceTime.Classes
import Particle.Test.Space3DCartesian_arbitrary
import Particle.BasicParticle
import Particle.ParametricParticle

-- | Arbitrary BasicParticles
instance Arbitrary BasicParticle where
  arbitrary = createParticle
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance (Space s, Arbitrary s) => Arbitrary (ParticleInSpace s) where
  arbitrary = createParticleInSpace
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary