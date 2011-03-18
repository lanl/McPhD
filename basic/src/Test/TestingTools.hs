{-# LANGUAGE TypeSynonymInstances #-}

module Test.TestingTools where

import Test.QuickCheck

import Particle
import Mesh
import PRNG
import Numerical
import Physical
import Data.Functor

-- | A newtype for aribtrary FP values in (0,1)
newtype Unit = Unit FP deriving (Show)
instance Arbitrary Unit where
  arbitrary = Unit <$> choose (0.0, 1.0)

instance Arbitrary RNG where
  arbitrary = mkRNG <$> arbitrary


sampleParticle :: Mesh -> RNG -> Tag -> Particle
sampleParticle mesh rng tag =
  let (r1, r2, r3, r4, r5, r6, rng') = getSixRNs rng
      (position, cell) = samplePosition mesh r1 r2 r3
      direction = sampleDirection mesh r4 r5 r6
  in Particle position direction (Time 1e300) (Energy 1.0) (EnergyWeight 1.0) cell rng' tag
