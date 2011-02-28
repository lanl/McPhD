{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Particle.BasicParticle where

import Particle.Classes

import qualified  Space3DCartesian as Space
import Data.Vector.V3
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Context and Environment

-- For these particles, the context is the end of the timestep.
data BContext = BContext
		{
		  -- | End of the time step.
		  basicContextTime :: Space.Time
		}

-- The environment is the same as the context.
data BEnv = BEnv
	    {
	      -- | End of the time step.
	      basicEnvTime :: Space.Time
	    }



-- * Particle definition and instance declarations

-- | BasicParticle is a RandomParticle moving in space and time.
data BasicParticle = BasicParticle
		     {
		       bpPos    :: Space.Position   -- ^ Position in space
		     , bpDir    :: Space.Direction  -- ^ Direction of travel
		     , bpSpeed  :: Space.Speed      -- ^ Magnitude of velocity
		     , bpTime   :: Space.Time       -- ^ Time in flight
		     , bpRand   :: PureMT           -- ^ Random number generator
		     } deriving Show


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
  move p d = p { bpPos = newPos }
      where newPos = Space.translate (position p) (direction p) d

instance InTime BasicParticle where
  time = bpTime
  tick p t = p { bpTime = newTime }
      where newTime = bpTime p + t

instance InSpaceTime BasicParticle where
  speed = bpSpeed

instance RandomParticle BasicParticle where
  type Random BasicParticle = PureMT
  getRandom = bpRand
  sample p  = (sample, p')
      where (sample, newRandom) = randomDouble (getRandom p)
	    p' = p { bpRand = newRandom }

instance Particle BasicParticle where
  type ContextT BasicParticle     = BContext
  type EnvironmentT BasicParticle = BEnv
  type EventT BasicParticle       = BasicEvent

  environment (BContext t) _ = BEnv t

  step environment particle = (event, particle')
    where time_left = (basicEnvTime environment) - (time particle)
	  particle' = advanceTime particle time_left
	  event     = StepEnd $ position particle'



-- * Events

-- | Reaching the end of the time step is the only event. Record the
-- position.
data BasicEvent = StepEnd { eventPosition :: Space.Position }

instance Event BasicEvent where
  type EventTally BasicEvent = PTally
  contribute event = PTally (eventPosition event)
  is_final _ = True  -- ^ Only event is final.




-- * Tallies

-- | The information we tally from each event is the position.
data PTally = PTally
	      {
		tallyPosition :: Space.Position -- ^ Final position
	      }

-- | The global tally is the sum of positions and a count of particles
data GPTally = GPTally
	       {
		 -- | Sum of final positions
		 positionSum :: Space.Position,

		 -- | Total number of particles
		 count :: Int
	       }

instance Tally GPTally where
    type TallyPart GPTally = PTally
    empty = GPTally{positionSum=Space.Position(Vector3 0 0 0), count=0}
    combine global event = GPTally sum' count'
	where sum' = Space.Position $ (Space.pos $ positionSum global) +
		     (Space.pos $ tallyPosition event)
	      count' = count global + 1





-- * Miscellaneous functions

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
