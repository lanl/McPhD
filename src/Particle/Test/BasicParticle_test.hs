-- | A testing module for BasicParticle
module Particle.Test.BasicParticle_test (tests) where

-- Testing Libraries
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck


-- The library under test
import Particle.BasicParticle

-- Its dependencies
import Particle.Test.Arbitrary
import Particle.Classes
import SpaceTime.Space3DCartesian as Space
import Approx

import Data.Vector.V3
import Control.Applicative


-- * Testing environment
-- | Environment:
test_environment = Space.Time 1.0

-- | Uniform particle origin:
origin = Space.Position ( Vector3 0.0 0.0 0.0 )

-- | Arbitrary BasicParticles
instance Arbitrary BasicParticle where
  arbitrary = createParticle
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

-- | Property: Particle clock should == 1.0 after one step.
prop_ParticleTime :: BasicParticle -> Bool
prop_ParticleTime p =
  let (_, p') = step test_environment p
  in bpTime p' ~== Space.Time 1.0

-- | Property: Distance traveled from orgin should be correct wrt speed & time.
prop_Distance :: BasicParticle -> Bool
prop_Distance p = undefined

tests =
  [
    testGroup "Step Operations"
    [
      testProperty "Correct final time" prop_ParticleTime
    ]
  ]