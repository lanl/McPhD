module Particle.Test.Particles_arbitrary where

import Control.Applicative
import Test.QuickCheck


import Space.Classes
import Particle.SpaceParticle

import Test.RandomNumbers_arbitrary ()

instance (Space s, Arbitrary s) => Arbitrary (SpaceParticle s) where
  arbitrary = createSpaceParticle
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary


-- TODO: Use Dmitry's approach to create instances for other particle
-- types that need  consistency between the cell and position.
