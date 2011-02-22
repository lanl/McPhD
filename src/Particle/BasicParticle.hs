{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | A module demonstrating random transport of a simple particle in a
-- mesh indexed with CellIndex.

module Particle.BasicParticle where

import Particle.Classes

import Space3DCartesian
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Particle and Event datatypes

data BasicParticle =
  InFlight { rpPos             :: Position   -- ^ Position in space
             , rpDir           :: Direction  -- ^ Direction of travel
             , rpSpeed         :: Speed      -- ^ Magnitude of velocity
             , rpTime          :: Time       -- ^ Time in flight
             , rpRand          :: PureMT     -- ^ Random number generator
           }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show

instance Approx BasicParticle where
  within_eps epsilon a b =
    and [
      (within_eps epsilon   (rpPos a)   (rpPos b))
      , (within_eps epsilon (rpDir a)   (rpDir b))
      , (within_eps epsilon (rpSpeed a) (rpSpeed b))
      , (within_eps epsilon (rpTime a)  (rpTime b))
      ]

-- The environment consists just of the end of time step.
data BasicEnvironment = BasicEnvironment { getEnvTime :: Time }

-- And this is the only event of interest.
data BasicEvent = StepEnd

instance Particle BasicParticle where
  type Environment BasicParticle = BasicEnvironment
  type Event BasicParticle       = BasicEvent

  -- | Get the properties and dispatch to implementation
  step env particle = step_impl (getEnvTime env) particle

-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position
                  -> Direction
                  -> Speed
                  -> Time
                  -> Seed
                  -> BasicParticle

createParticle pos dir speed time seed =
  InFlight pos dir speed time (makePureMT seed)

-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT
                     -> Position
                     -> Speed
                     -> Time
                     -> BasicParticle
sampleIsoParticle rand position speed time =
  let (direction, rand') = randomDirection rand
  in InFlight position direction speed time rand'



step_impl :: Time -> BasicParticle -> Maybe (BasicEvent , BasicParticle)
step_impl = undefined