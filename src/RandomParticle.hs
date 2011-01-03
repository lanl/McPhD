{-# LANGUAGE BangPatterns #-}
-- | A module demonstrating random transport of a simple particle in
-- homogeneous space without a mesh. 
module RandomParticle where

import Space
import RandomValues

import Data.List
import System.Random.Mersenne.Pure64
import Data.Vector.Class

-- * Particle and Event datatypes

data RandomParticle = InFlight { rpPos  :: Position,  -- ^ Position in space
                                 rpDir  :: Direction, -- ^ Direction of travel
                                 rpDist :: Distance,  -- ^ Remaining distance of travel
                                 rpRand :: PureMT     -- ^ Random number generator
                               } 
                    | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
                    deriving Show
                      

-- | Create a particle with given position, direction, distance and random seed.
createParticle :: Position -> Direction -> Distance -> Integer -> RandomParticle                  
createParticle pos dir dist seed = InFlight pos dir dist (pureMT $ fromIntegral seed)

-- | Create a particle with given generator, position and
-- distance. Sample an isotropic initial direction.
sampleIsoParticle :: PureMT -> Position -> Distance -> RandomParticle
sampleIsoParticle rand position distance = let
  (direction, rand') = randomDirection rand
  in InFlight position direction distance rand'




-- | Each event is motion of a particle & a Limiter which stopped it.
data Event = Event { motion::Motion, limit::Limiter } deriving Show

-- | Limiters are things which stop a particle's motion.
data Limiter = 
  Scatter Direction -- ^ Scatter with momentum vector
  | Escape RandomParticle -- ^ Particle escapes the problem domain
  | Termination RandomParticle -- ^ Particle reaches end-of-life
    deriving Show
             
-- | Motion consists of a direction and distance. (This assumes that
-- Direction is normalized)
data Motion = Motion { motionDir :: Direction, motionDist :: Distance } deriving Show

-- | A physical property of the space which determines streaming distances.
newtype Opacity = Opacity { opValue :: Double }




-- * Functions for various motions and events on particles

-- | Change a particle's direction of motion isotropically. 
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = InFlight position direction distance rand
  where position = rpPos p
        distance = rpDist p
        (direction, rand) = randomDirection (rpRand p)
         
-- | Translate a particle in space.
translateRP :: RandomParticle -> Distance -> RandomParticle
translateRP Dead _ = Dead
translateRP p d = InFlight position direction distance rand
  where position  = translate (rpPos p) (rpDir p) d
        direction = rpDir p
        distance  = rpDist p - d
        rand      = rpRand p

-- | A composite operation for motion and scattering
translateAndScatterRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndScatterRP particle distance = (event, particle') where 
  motion    = Motion (rpDir particle) distance
  particle' = scatterRP $ translateRP particle distance
  momentum  = Direction ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = Event motion (Scatter momentum)

-- | A composite event for motion and termination
translateAndTerminateRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndTerminateRP particle distance = (event, Dead) where
  motion    = Motion (rpDir particle) distance
  particle' = translateRP particle distance
  event     = Event motion (Termination particle')


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


step :: Opacity -> RandomParticle -> Maybe (Event, RandomParticle)
step _ Dead = Nothing
step opacity particle = 
  let (scatter_distance, randState) = randomExponential (opValue opacity) (rpRand particle) 
  in stepRP particle{rpRand = randState} scatter_distance
     
     

-- * Simulation Functions

stream :: Opacity -> RandomParticle -> [Event]
stream opacity initial_particle = unfoldr (step opacity) initial_particle
