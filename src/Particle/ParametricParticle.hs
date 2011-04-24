{-
A particle type which is paramaterized over space.
-}
module Particle.ParametricParticle where

import System.Random.Mersenne.Pure64


import SpaceTime.Classes
import RandomNumbers
import Numerics
import Approx

import SpaceTime.Test.Space_arbitrary

data ParametricParticle space =
  ParametricParticle
  {
    ppLocation :: space -- ^ Where is it?
  , ppTime :: Time      -- ^ Time in flight.
  , ppRand :: PureMT    -- ^ RNG.
  } deriving Show

createParametricParticle :: (Space a) => a -> Time -> Seed -> ParametricParticle a
createParametricParticle location time seed =
  ParametricParticle location time (makePureMT seed)

instance (Approx a) => Approx (ParametricParticle a) where
  within_eps epsilon a b =
    (weps (ppLocation a) (ppLocation b)) && (weps (ppLocation a) (ppLocation b))
    where weps = within_eps epsilon
