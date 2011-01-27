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
  InFlight { rpPos   :: Position     -- ^ Position in space
	     , rpDir   :: Direction  -- ^ Direction of travel
	     , rpDist  :: Distance   -- ^ Remaining distance of travel
	     , rpIndex :: Cell       -- ^ Cell index
	     , rpRand  :: PureMT     -- ^ Random number generator
	   }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show


instance Approx RandomParticle where
  within_eps epsilon a b = all id [ (within_eps epsilon (rpPos a)  (rpPos b)),
				    (within_eps epsilon (rpDir a)  (rpDir b)),
				    (within_eps epsilon (rpDist a) (rpDist b)),
				    (rpIndex a == rpIndex b)
				  ]


-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position -> Direction -> Distance
		  -> Cell -> Integer -> RandomParticle
createParticle pos dir dist cell seed =
  InFlight pos dir dist cell (pureMT $ fromIntegral seed)

-- | Create a particle with given generator, position and
-- distance and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT -> Position -> Distance -> Cell -> RandomParticle
sampleIsoParticle rand position distance cell = let
  (direction, rand') = randomDirection rand
  in InFlight position direction distance cell rand'


-- | Each event is motion of a particle & a Limiter which stopped it.
data Event = Event { eventMotion::Motion, eventLimit::Limiter } deriving Show

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

-- | Change a particle's direction of motion isotropically.
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = p{rpDir=direction, rpRand=rand}
  where (direction, rand) = randomDirection (rpRand p)

nextCellRP :: SimpleMesh -> Face -> RandomParticle -> RandomParticle
nextCellRP mesh face particle = particle{rpIndex = nextCell mesh face}

-- | Translate a particle in space.
translateRP :: RandomParticle -> Distance -> RandomParticle
translateRP Dead _ = Dead
translateRP p d = p{rpPos = position, rpDist = distance}
  where position  = translate (rpPos p) (rpDir p) d
	distance  = rpDist p - d

-- | A composite operation for motion and scattering
translateAndScatterRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndScatterRP particle distance = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = scatterRP $ translateRP particle distance
  momentum  = Momentum ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = Event motion' (Scatter momentum)

-- | A compositie operation for motion and face crossing
translateToFaceRP :: SimpleMesh ->
		     RandomParticle ->
		     Distance ->
		     Face -> (Event, RandomParticle)
translateToFaceRP mesh particle distance face = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = nextCellRP mesh face $ translateRP particle distance
  event     = Event motion' (FaceCrossing face)



-- | A composite event for motion and termination
translateAndTerminateRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndTerminateRP particle distance = (event, Dead) where
  motion'   = motion (rpDir particle) distance
  particle' = translateRP particle distance
  event     = Event motion' (Termination particle')


-- * Step functions

-- | Step operation, with a given potential distance of travel
stepRP :: RandomParticle -> Distance -> Maybe (Event, RandomParticle)
stepRP Dead _ = Nothing
stepRP particle distance = Just (event, particle') where
  remaining = rpDist particle
  (event, particle') = if distance < remaining then
			 translateAndScatterRP particle distance
		       else
			 translateAndTerminateRP particle remaining
