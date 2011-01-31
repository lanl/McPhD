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
<<<<<<< local
	     , rpSpeed         :: Speed      -- ^ Magnitude of velocity
	     , rpTime          :: Time       -- ^ Time in flight
=======
	     , rpDistToEnd     :: Distance   -- ^ Remaining distance of travel
>>>>>>> other
	     , rpMFPToScatter  :: Distance   -- ^ Distance in MFP to next scatter
	     , rpIndex         :: Cell       -- ^ Cell index
	     , rpRand          :: PureMT     -- ^ Random number generator
	   }
  | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
  deriving Show


instance Approx RandomParticle where
  within_eps epsilon a b =
    all id [
<<<<<<< local
      (within_eps epsilon   (rpPos a) (rpPos b))
=======
      (within_eps epsilon (rpPos a) (rpPos b))
>>>>>>> other
      , (within_eps epsilon (rpDir a) (rpDir b))
<<<<<<< local
      , (within_eps epsilon (rpSpeed a) (rpSpeed b))
      , (within_eps epsilon (rpTime a) (rpTime b))
=======
      , (within_eps epsilon (rpDistToEnd a) (rpDistToEnd b))
>>>>>>> other
      , (within_eps epsilon (rpMFPToScatter a) (rpMFPToScatter b))
      , (rpIndex a == rpIndex b)
      ]

-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position
		  -> Direction
<<<<<<< local
		  -> Speed
		  -> Time
=======
>>>>>>> other
		  -> Distance
<<<<<<< local
		  -> CellIndex
=======
		  -> Distance
		  -> Cell
>>>>>>> other
		  -> Seed
		  -> RandomParticle
<<<<<<< local
createParticle pos dir speed time distScatter cell seed =
  InFlight pos dir speed time distScatter (Local cell) (makePureMT seed)
=======
createParticle pos dir distEnd distScatter cell seed =
  InFlight pos dir distEnd distScatter cell (makePureMT seed)
>>>>>>> other

<<<<<<< local
-- | Create a particle with given position, time, distance remaining
-- and cell. Sample an isotropic initial direction.
=======
-- | Create a particle with given generator, position and
-- distance and cell. Sample an isotropic initial direction.
>>>>>>> other
sampleIsoParticle :: PureMT
		     -> Position
<<<<<<< local
		     -> Speed
		     -> Time
=======
>>>>>>> other
		     -> Distance
<<<<<<< local
		     -> CellIndex
=======
		     -> Distance
		     -> Cell
>>>>>>> other
		     -> RandomParticle
<<<<<<< local
sampleIsoParticle rand position speed time distScatter cell =
=======
sampleIsoParticle rand position distEnd distScatter cell =
>>>>>>> other
  let (direction, rand') = randomDirection rand
<<<<<<< local
  in InFlight position direction speed time distScatter (Local cell) rand'
=======
  in InFlight position direction distEnd distScatter cell rand'
>>>>>>> other


-- | Each event is motion of a particle & a Limiter which stopped it.
data Event = Event
	     { eventMotion::Motion
	     , eventLimit::Limiter
	     } deriving Show

-- | Limiters are things which stop a particle's motion.
data Limiter =
  Scatter Momentum             -- ^ Scatter with momentum vector
  | Escape RandomParticle      -- ^ Particle escapes the projblem domain
  | Termination RandomParticle -- ^ Particle reaches end-of-life
  | FaceCrossing Face          -- ^ Particle crossed a cell face
  deriving Show


-- * Functions for various motions and events on particles

-- TODO: These are highly repetitive, and they repeat themselves. Look
-- for some way of combining basic functions into these operations.

-- | A physical property of the space which determines streaming distances.
<<<<<<< local
newtype Opacity = Opacity { opValue :: Double } deriving (Eq, Num, Show)
=======
newtype Opacity = Opacity { opValue :: Double }
>>>>>>> other

-- | Change a particle's direction of motion isotropically.
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = p{rpDir=direction, rpRand=rand}
  where (direction, rand) = randomDirection (rpRand p)

nextCellRP :: SimpleMesh
<<<<<<< local
	     -> Face
	     -> RandomParticle
	     -> RandomParticle
=======
	      -> Face
	      -> RandomParticle
	      -> RandomParticle
>>>>>>> other
nextCellRP mesh face particle = particle{rpIndex = nextCell mesh face}


-- | Translate a particle in space.
translateRP :: Opacity
	       -> Distance
	       -> RandomParticle
	       -> RandomParticle
translateRP _ _ Dead = Dead
translateRP opacity distance particle =
  particle
  {
    rpPos          = position'
<<<<<<< local
  , rpTime         = time'
=======
  , rpDistToEnd    = distToEnd'
>>>>>>> other
  , rpMFPToScatter = mfpToEnd'
  }
  where
    position'  = translate (rpPos particle) (rpDir particle) distance
<<<<<<< local
    time'      = rpTime particle + timeToDistance distance (rpSpeed particle)
    mfpToEnd'  = rpMFPToScatter particle -
		 (Distance $ (dis distance)*(opValue opacity))
=======
    distToEnd' = rpDistToEnd particle - distance
    mfpToEnd'  = rpMFPToScatter particle - (Distance $ (dis distance)*(opValue opacity))
>>>>>>> other

-- | A composite operation for motion and scattering
translateAndScatterRP :: Opacity
			 -> Distance
			 -> RandomParticle
			 -> (Event, RandomParticle)
translateAndScatterRP opacity distance particle = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = scatterRP $ translateRP opacity distance particle
  momentum  = Momentum ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = Event motion' (Scatter momentum)

-- | A compositie operation for motion and face crossing
translateToFaceRP :: SimpleMesh
		     -> Opacity
		     -> Distance
		     -> Face
		     -> RandomParticle
		     -> (Event, RandomParticle)
<<<<<<< local
translateToFaceRP mesh opacity distance face particle =
  let motion'    = motion (rpDir particle) distance
      particle'  = translateRP opacity distance particle
      particle'' = nextCellRP mesh face particle'
      event      = Event motion' (FaceCrossing face)
  in (event, particle'')
=======
translateToFaceRP mesh opacity distance face particle = (event, particle') where
  motion'   = motion (rpDir particle) distance
  particle' = nextCellRP mesh face $ translateRP opacity distance particle
  event     = Event motion' (FaceCrossing face)

>>>>>>> other


-- | A composite event for motion and termination
translateAndTerminateRP :: Opacity
			   -> Distance
			   -> RandomParticle
			   -> (Event, RandomParticle)
translateAndTerminateRP opacity distance particle = (event, Dead) where
  motion'   = motion (rpDir particle) distance
  particle' = translateRP opacity distance particle
  event     = Event motion' (Termination particle')


-- * Step functions

-- | Step operation, with a given potential distance of travel
stepRP :: Opacity
<<<<<<< local
	  -> Time           -- ^ End of timestep time
	  -> Distance       -- ^ Tentative travel distance
	  -> RandomParticle -- ^ Particle to Step
=======
	  -> Distance
	  -> RandomParticle
>>>>>>> other
	  -> Maybe (Event, RandomParticle)
<<<<<<< local
stepRP _ _ _ Dead = Nothing
stepRP opacity endtime distance particle = Just (event, particle') where
  distance_to_step_end = distanceToTime (endtime - rpTime particle) (rpSpeed particle)
  (event, particle') = if distance < distance_to_step_end then
=======
stepRP _ _ Dead = Nothing
stepRP opacity distance particle = Just (event, particle') where
  remaining = rpDistToEnd particle
  (event, particle') = if distance < remaining then
>>>>>>> other
			 translateAndScatterRP opacity distance particle
		       else
<<<<<<< local
			 translateAndTerminateRP opacity distance_to_step_end particle
=======
			 translateAndTerminateRP opacity remaining particle
>>>>>>> other
