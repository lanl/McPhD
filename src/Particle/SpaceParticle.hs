{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}
module Particle.SpaceParticle where

import Data.Function
import System.Random.Mersenne.Pure64

import Space.Classes

import RandomNumbers
import Properties
import Approx

import Space.Test.Space_arbitrary ()

-- | Data type for a particle moving through space. No mesh or mesh index.
data SpaceParticle space = SpaceParticle
    {
      ppLocation :: space  -- ^ Location in Space
    , ppTime     :: Time   -- ^ Time in flight.
    , ppSpeed    :: Speed  -- ^ Speed of motion
    , ppRand     :: PureMT -- ^ RNG.
    } deriving Show

createSpaceParticle :: (Space a) => a -> Time -> Speed -> Seed -> SpaceParticle a
createSpaceParticle location time speed seed =
  SpaceParticle location time speed (makePureMT seed)

instance (Approx space) => Approx (SpaceParticle space) where
  within_eps epsilon a b = (weps `on` ppLocation) a b &&
                           (weps `on` ppTime) a b
    where weps = within_eps epsilon
