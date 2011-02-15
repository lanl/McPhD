{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A module demonstrating random transport of a simple particle in a
-- mesh indexed with CellIndex.

module Particle.RandomParticle where

import Space3DCartesian
import Mesh.SimpleCartesian
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Particle and Event datatypes

data RandomParticle =
  InFlight { rpPos             :: Position   -- ^ Position in space
	     , rpDir           :: Direction  -- ^ Direction of travel
	     , rpSpeed         :: Speed      -- ^ Magnitude of velocity
	     , rpTime          :: Time       -- ^ Time in flight
	     , rpIndex         :: Cell       -- ^ Cell index
	     , rpRand          :: PureMT     -- ^ Random number generator
	   }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show

instance Approx RandomParticle where
<<<<<<< local
  within_eps epsilon a b = all id [ 
                                (within_eps epsilon (rpPos a) (rpPos b))
                               , (within_eps epsilon (rpDir a) (rpDir b))
                               , (within_eps epsilon (rpDistToEnd a) (rpDistToEnd b))
                               , (within_eps epsilon (rpDistToScatter a) (rpDistToScatter b))
                               , (rpIndex a == rpIndex b)
			       ]
-- | Create a particle with given position, direction, distance and random seed. 
createParticle :: Position 
                  -> Direction 
                  -> Distance 
                  -> Distance 
                  -> Cell 
                  -> Seed
                  -> RandomParticle
createParticle pos dir distEnd distScatter cell seed =
  InFlight pos dir distEnd distScatter cell (makePureMT seed)
=======
  within_eps epsilon a b =
    and [
      (within_eps epsilon   (rpPos a) (rpPos b))
      , (within_eps epsilon (rpDir a) (rpDir b))
      , (within_eps epsilon (rpSpeed a) (rpSpeed b))
      , (within_eps epsilon (rpTime a) (rpTime b))
      , (rpIndex a == rpIndex b)
      ]

-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position
		  -> Direction
		  -> Speed
		  -> Time
		  -> CellIndex
		  -> Seed
		  -> RandomParticle
>>>>>>> other


createParticle pos dir speed time cell seed =
  InFlight pos dir speed time (Local cell) (makePureMT seed)

-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT
		     -> Position
		     -> Speed
		     -> Time
		     -> CellIndex
		     -> RandomParticle
sampleIsoParticle rand position speed time cell =
  let (direction, rand') = randomDirection rand
  in InFlight position direction speed time (Local cell) rand'


-- | Each event is motion of a particle & a Limiter which stopped it.
data Event = Event { 
    eventMotion::Motion
    , eventLimit::Limiter
    } deriving Show

-- | Limiters are things which stop a particle's motion.
data Limiter =
  Scatter Momentum             -- ^ Scatter with momentum vector
  | Escape RandomParticle      -- ^ Particle escapes the problem domain
  | Termination RandomParticle -- ^ Particle reaches end-of-life
  | FaceCrossing Face          -- ^ Particle crossed a cell face
  deriving Show


-- * Functions for various motions and events on particles

-- TODO: These are highly repetitive, and they repeat themselves. Look
-- for some way of combining basic functions into these operations.

-- | A physical property of the space which determines streaming distances.
-- newtype Opacity = Opacity { opValue :: Double } deriving (Eq, Num, Show)

-- | Change a particle's direction of motion isotropically.
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = p{rpDir=direction, rpRand=rand}
  where (direction, rand) = randomDirection (rpRand p)

nextCellRP :: SimpleMesh
	      -> Face
	      -> RandomParticle
	      -> RandomParticle
nextCellRP mesh face particle = particle{rpIndex = nextCell mesh face}

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
			 -> (Event, RandomParticle)
translateAndScatterRP distance particle = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = scatterRP $ translateRP distance particle
  momentum  = Momentum ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = Event motion' (Scatter momentum)

-- | A compositie operation for motion and face crossing
translateToFaceRP :: SimpleMesh
		     -> Distance
		     -> Face
		     -> RandomParticle
		     -> (Event, RandomParticle)
translateToFaceRP mesh distance face particle =
  let motion'    = motion (rpDir particle) distance
      particle'  = translateRP distance particle
      particle'' = nextCellRP mesh face particle'
      event      = Event motion' (FaceCrossing face)
  in (event, particle'')


-- | A composite event for motion and termination
translateAndTerminateRP :: Distance
			   -> RandomParticle
			   -> (Event, RandomParticle)
translateAndTerminateRP distance particle = (event, Dead) where
  motion'   = motion (rpDir particle) distance
  particle' = translateRP distance particle
  event     = Event motion' (Termination particle')


-- * Step functions

-- | Step operation, with a given potential distance of travel
stepRP :: Time              -- ^ End of timestep
	  -> Distance       -- ^ Tentative travel distance
	  -> RandomParticle -- ^ Particle to Step
	  -> Maybe (Event, RandomParticle)

stepRP _ _ Dead = Nothing
stepRP endtime distance particle = Just (event, particle') where
  distance_to_step_end = distanceToTime (endtime - rpTime particle) (rpSpeed particle)
  (event, particle') = if distance < distance_to_step_end then
			 translateAndScatterRP distance particle
		       else
			 translateAndTerminateRP distance_to_step_end particle

-- | A simpler step function which only updates time and space quantities
step' :: Distance -> RandomParticle ->  (Motion, RandomParticle)
step' distance particle = undefined
