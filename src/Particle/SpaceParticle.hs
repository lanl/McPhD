{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances, NoMonomorphismRestriction, StandaloneDeriving #-}
module Particle.SpaceParticle where

import Data.Function

import Coordinate.Classes
import RandomNumbers
import Properties
import Approx

import Coordinate.Test.Arbitrary ()

-- | Data type for a particle moving through space. No mesh or mesh index.
data SpaceParticle space = SpaceParticle
    {
      ppLocation :: space  -- ^ Location in Space
    , ppTime     :: Time   -- ^ Time in flight.
    , ppSpeed    :: Speed  -- ^ Speed of motion
    , ppRand     :: RNG
    } deriving Show

createSpaceParticle :: (Coordinate a) => a -> Time -> Speed -> Seed -> SpaceParticle a
createSpaceParticle location time speed seed =
  SpaceParticle location time speed (makeRNG seed)

instance (Approx space) => Approx (SpaceParticle space) where
  within_eps epsilon a b = (weps `on` ppLocation) a b &&
                           (weps `on` ppTime) a b
    where weps = within_eps epsilon
