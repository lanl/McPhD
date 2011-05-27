module Particle.Test.Particles_arbitrary where

import Control.Applicative
import Test.QuickCheck


import Space.Classes
import Particle.ParametricParticle

import Test.RandomNumbers_arbitrary ()

instance (Space s, Arbitrary s) => Arbitrary (ParticleInSpace s) where
  arbitrary = createParticleInSpace
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              

-- TODO: Use Dmitry's approach to create instances for other particle
-- types that need  consistency between the cell and position.
  
              