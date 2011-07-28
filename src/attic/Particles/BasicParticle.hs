{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Particle.BasicParticle where

import Particle.Classes

import qualified Coordinate.Space3DCartesian as Space
import Data.Vector.V3
import RandomNumbers
import Approx

import System.Random.Mersenne.Pure64


-- * Particle definition and instance declarations

-- | BasicParticle is a particle with random state moving in space and time.
data BasicParticle = BasicParticle
                     {
                       bpPos    :: Coordinate.Position   -- ^ Position in space
                     , bpDir    :: Coordinate.Direction  -- ^ Direction of travel
                     , bpSpeed  :: Coordinate.Speed      -- ^ Magnitude of velocity
                     , bpTime   :: Coordinate.Time       -- ^ Time in flight
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
      where newPos = Coordinate.translate (position p) (direction p) d

instance InTime BasicParticle where
  time = bpTime
  tick p t = p { bpTime = newTime }
      where newTime = bpTime p + t

instance InSpace BasicParticle where
  speed = bpSpeed

instance RandomParticle BasicParticle where
  type Random BasicParticle = PureMT
  getRandom = bpRand
  sample p  = (sample, p')
      where (sample, newRandom) = randomDouble (getRandom p)
            p' = p { bpRand = newRandom }

instance Particle BasicParticle where
  type ContextT BasicParticle     = Coordinate.Time
  type EnvironmentT BasicParticle = Coordinate.Time
  type EventT BasicParticle       = Coordinate.Position

  environment t _ = t

  step envtime particle = (event, particle')
    where time_left = envtime - time particle
          particle' = advanceTime particle time_left
          event     = position particle'

-- * Events

-- | Reaching the end of the time step is the only event. Record the
-- position.
type BasicEvent = Coordinate.Position

instance Event BasicEvent where
  -- | The information we tally from each event is the (final) position.
  type EventTally BasicEvent = Coordinate.Position
  contribute event = event
  is_final _ = True  -- ^ Only event is final.


-- | Compute the distance to and result of the event:
toBasicEvent :: Coordinate.Time
                -> BasicParticle
                -> (Coordinate.Distance, (Coordinate.Position, BasicParticle) )
toBasicEvent envtime particle = (distance, (event, particle'))
  where time_left = envtime - time particle
        particle' = advanceTime particle time_left
        distance  = Coordinate.distanceToTime time_left ( speed particle )
        event     = position particle'



-- * Tallies

-- | The global tally is the sum of positions and a count of particles
data GPTally = GPTally
               {
                 -- | Sum of final positions
                 positionSum :: Coordinate.Position,

                 -- | Total number of particles
                 count :: Int
               }

instance Tally GPTally where
    type TallyEvent GPTally = Coordinate.Position
    empty = GPTally{positionSum=Coordinate.Position(Vector3 0 0 0), count=0}
    combine pos gp = GPTally sum' count'
        where sum'   = positionSum gp + pos
              count' = count gp + 1





-- * Miscellaneous functions

-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Coordinate.Position
               -> Coordinate.Direction
               -> Coordinate.Speed
               -> Coordinate.Time
               -> Seed
               -> BasicParticle

createParticle pos dir speed time seed =
  BasicParticle pos dir speed time (makePureMT seed)

-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT
                     -> Coordinate.Position
                     -> Coordinate.Speed
                     -> Coordinate.Time
                     -> BasicParticle
sampleIsoParticle rand position speed time =
  let (direction, rand') = Coordinate.randomDirection rand
  in BasicParticle position direction speed time rand'
