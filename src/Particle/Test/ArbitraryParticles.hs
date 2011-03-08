module Particle.Test.ArbitraryParticles where

import Particle.Test.Arbitrary
import Control.Applicative
import Test.QuickCheck

import Particle.RandomParticle

instance Arbitrary RandomParticle where
  arbitrary = createParticle
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
