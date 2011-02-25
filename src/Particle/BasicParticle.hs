{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | A module demonstrating random transport of a simple particle in a
-- mesh indexed with CellIndex.

module Particle.BasicParticle where

import Particle.Classes

import qualified  Space3DCartesian as Space
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Particle and Event datatypes

-- | BasicParticle is a RandomParticle moving in space and time.
data BasicParticle = BasicParticle { 
      bpPos    :: Space.Position   -- ^ Position in space
    , bpDir    :: Space.Direction  -- ^ Direction of travel
    , bpSpeed  :: Space.Speed      -- ^ Magnitude of velocity
    , bpTime   :: Space.Time       -- ^ Time in flight
    , bpRand   :: PureMT           -- ^ Random number generator
    } deriving Show


data BasicEnvironment = BasicEnvironment 
    { 
      basicEnvTime :: Space.Time -- ^ End of time step.
    }

-- For these particles, the environment is equal to the context.
data BasicContext = BasicContext { basicContextTime :: Space.Time }

getEnvironment :: BasicContext -> BasicEnvironment
getEnvironment (BasicContext time) = BasicEnvironment time

-- And this is the only event of interest. It records the final
-- position of the particle.
data BasicEvent = StepEnd { eventPosition :: Space.Position }

-- The information we tally from each event is the position.
data PositionTally = PositionTally { tallyPosition :: Space.Position }

-- The global tally is the sum of positions and a count of contributions
data GlobalPositionTally = GlobalPositionTally { positionSum :: Space.Position,  count :: Int }

-- * Class Instances


instance Approx BasicParticle where
  within_eps epsilon a b =
    and [
      (within_eps epsilon (bpPos a)   (bpPos b))
    , (within_eps epsilon (bpDir a)   (bpDir b))
    , (within_eps epsilon (bpSpeed a) (bpSpeed b))
    , (within_eps epsilon (bpTime a)  (bpTime b))
      ]

instance InSpace BasicParticle where
  position  = bpPos
  direction = bpDir
  move p d  = p { bpPos = newPos }
      where newPos = Space.translate (position p) (direction p) d

instance InTime BasicParticle where
  time     = bpTime
  tick p t = p { bpTime = newTime }
      where newTime = bpTime p + t
            
instance InSpaceTime BasicParticle where
  speed     = bpSpeed

instance RandomParticle BasicParticle where
  type Random BasicParticle = PureMT
  getRandom = bpRand
  sample p  = (sample, p')
      where (sample, newRandom) = randomDouble (bpRand p)
            p' = p { bpRand = newRandom }


instance Particle BasicParticle where
  type ContextT BasicParticle     = BasicContext
  type EnvironmentT BasicParticle = BasicEnvironment
  type EventT BasicParticle       = BasicEvent
      
  -- | Environment = Context
  environment c _ = getEnvironment c
      
  -- | Get the properties and dispatch to implementation
  step environment particle = (event, particle')
    where time_left = (basicEnvTime environment) - (time particle)
          particle' = advanceTime particle time_left
          event     = StepEnd $ position particle'

instance Event BasicEvent where
  type EventTally BasicEvent = PositionTally
  contribute event = PositionTally (eventPosition event)
  is_final _ = True

instance Tally GlobalPositionTally where
    type TallyPart GlobalPositionTally = PositionTally
    combine global event = GlobalPositionTally sum' count'
        where sum' = Space.Position $ (Space.pos $ positionSum global) + (Space.pos $ tallyPosition event)
              count' = count global + 1

    
-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Space.Position
               -> Space.Direction
               -> Space.Speed
               -> Space.Time
               -> Seed
               -> BasicParticle

createParticle pos dir speed time seed =
  BasicParticle pos dir speed time (makePureMT seed)

-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT
                     -> Space.Position
                     -> Space.Speed
                     -> Space.Time
                     -> BasicParticle
sampleIsoParticle rand position speed time =
  let (direction, rand') = randomDirection rand
  in BasicParticle position direction speed time rand'
