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
	     , rpDistToEnd     :: Distance   -- ^ Remaining distance of travel
	     , rpDistToScatter :: Distance   -- ^ Distance in MFP to next scatter
             , rpIndex         :: Cell       -- ^ Cell index
	     , rpRand          :: PureMT     -- ^ Random number generator
	   }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show


instance Approx RandomParticle where
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

-- | Create a particle with given generator, position and
-- distance and cell. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT 
                     -> Position 
                     -> Distance 
                     -> Distance 
                     -> Cell 
                     -> RandomParticle
sampleIsoParticle rand position distEnd distScatter cell = let
  (direction, rand') = randomDirection rand
  in InFlight position direction distEnd distScatter cell rand'


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
translateRP distance particle = particle{rpPos = position, rpDistToEnd = distance'}
  where position  = translate (rpPos particle) (rpDir particle) distance
	distance' = rpDistToEnd particle - distance

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
translateToFaceRP mesh distance face particle = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = nextCellRP mesh face $ translateRP distance particle
  event     = Event motion' (FaceCrossing face)



-- | A composite event for motion and termination
translateAndTerminateRP :: Distance -> RandomParticle -> (Event, RandomParticle)
translateAndTerminateRP distance particle = (event, Dead) where
  motion'   = motion (rpDir particle) distance
  particle' = translateRP distance particle
  event     = Event motion' (Termination particle')


-- * Step functions

-- | Step operation, with a given potential distance of travel
stepRP :: Distance -> RandomParticle -> Maybe (Event, RandomParticle)
stepRP _ Dead = Nothing
stepRP distance particle = Just (event, particle') where
  remaining = rpDistToEnd particle
  (event, particle') = if distance < remaining then
			 translateAndScatterRP distance particle
		       else
			 translateAndTerminateRP remaining particle
