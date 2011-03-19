-- | A module demonstrating random transport of a simple particle in a
-- mesh indexed with CellIndex.

module Particle.RandomParticle where

import Space3DCartesian
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Particle and Event datatypes

data RandomParticle =
  InFlight { rpPos             :: Position   -- ^ Position in space
             , rpDir           :: Direction  -- ^ Direction of travel
             , rpSpeed         :: Speed      -- ^ Magnitude of velocity
             , rpTime          :: Time       -- ^ Time in flight
             , rpRand          :: PureMT     -- ^ Random number generator
           }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show

instance Approx RandomParticle where
  within_eps epsilon a b =
    and [
      (within_eps epsilon   (rpPos a)   (rpPos b))
      , (within_eps epsilon (rpDir a)   (rpDir b))
      , (within_eps epsilon (rpSpeed a) (rpSpeed b))
      , (within_eps epsilon (rpTime a)  (rpTime b))
      ]


-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position
                  -> Direction
                  -> Speed
                  -> Time
                  -> Seed
                  -> RandomParticle

createParticle pos dir speed time seed =
  InFlight pos dir speed time (makePureMT seed)

-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT
                     -> Position
                     -> Speed
                     -> Time
                     -> RandomParticle
sampleIsoParticle rand position speed time =
  let (direction, rand') = randomDirection rand
  in InFlight position direction speed time rand'


-- | Limiters are things which stop a particle's motion.
data RandomLimiter =
  Scatter Momentum             -- ^ Scatter with momentum vector
  | Escape RandomParticle      -- ^ Particle escapes the problem domain
  | Termination RandomParticle -- ^ Particle reaches end-of-life
  deriving Show


-- | Each event is motion of a particle & a Limiter which stopped it.
data RandomEvent = RandomEvent {
    eventMotion::Motion
  , eventLimit::RandomLimiter
  } deriving Show



-- * Functions for various motions and events on particles

-- | Change a particle's direction of motion isotropically.
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = p{rpDir=direction, rpRand=rand}
  where (direction, rand) = randomDirection (rpRand p)


-- | Translate a particle in space.
translateRP :: Distance
            -> RandomParticle
            -> RandomParticle
translateRP _ Dead = Dead
translateRP distance particle =
  particle
  {
    rpPos  = position'
  , rpTime = time'
  }
  where
    position'  = translate (rpPos particle) (rpDir particle) distance
    time'      = rpTime particle + timeToDistance distance (rpSpeed particle)

-- | A composite operation for motion and scattering
translateAndScatterRP :: Distance
                      -> RandomParticle
                      -> (RandomEvent, RandomParticle)
translateAndScatterRP distance particle = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = scatterRP $ translateRP distance particle
  momentum  = Momentum ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = RandomEvent motion' (Scatter momentum)


-- | A composite event for motion and termination
translateAndTerminateRP :: Distance
                        -> RandomParticle
                        -> (RandomEvent, RandomParticle)
translateAndTerminateRP distance particle = (event, Dead) where
  motion'   = motion (rpDir particle) distance
  particle' = translateRP distance particle
  event     = RandomEvent motion' (Termination particle')


-- | Step operation, with a given potential distance of travel
stepRP :: Time           -- ^ End of timestep
       -> Distance       -- ^ Tentative travel distance
       -> RandomParticle -- ^ Particle to Step
       -> Maybe (RandomEvent, RandomParticle)

stepRP _ _ Dead = Nothing
stepRP endtime distance particle = Just (event, particle') where
  distance_to_step_end = distanceToTime (endtime - rpTime particle) (rpSpeed particle)
  (event, particle') = if distance < distance_to_step_end then
                         translateAndScatterRP distance particle
                       else
                         translateAndTerminateRP distance_to_step_end particle
