-- | A module demonstrating random transport of a simple particle in
-- homogeneous space without a mesh. 
module Particle.RandomParticle where

import Space
import RandomValues
import Approx

import System.Random.Mersenne.Pure64

-- * Particle and Event datatypes

data RandomParticle = InFlight { rpPos  :: Position,  -- ^ Position in space
                                 rpDir  :: Direction, -- ^ Direction of travel
                                 rpDist :: Distance,  -- ^ Remaining distance of travel
                                 rpRand :: PureMT     -- ^ Random number generator
                               } 
                    | Dead  -- ^ Pining for the Fjords. Used to terminate the unfold.
                    deriving Show
                      

instance Approx RandomParticle where
  within_eps epsilon a b = all id [ (within_eps epsilon (rpPos a)  (rpPos b)), 
                                    (within_eps epsilon (rpDir a)  (rpDir b)), 
                                    (within_eps epsilon (rpDist a) (rpDist b))]


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
  Scatter Momentum             -- ^ Scatter with momentum vector
  | Escape RandomParticle      -- ^ Particle escapes the problem domain
  | Termination RandomParticle -- ^ Particle reaches end-of-life
    deriving Show
             

-- * Functions for various motions and events on particles

-- | Change a particle's direction of motion isotropically. 
scatterRP :: RandomParticle -> RandomParticle
scatterRP Dead = Dead
scatterRP p = p{rpDir=direction, rpRand=rand}
  where (direction, rand) = randomDirection (rpRand p)
         
-- | Translate a particle in space.
translateRP :: RandomParticle -> Distance -> RandomParticle
translateRP Dead _ = Dead
translateRP p d = p{rpPos = position, rpDist = distance}
  where position  = translate (rpPos p) (rpDir p) d
        distance  = rpDist p - d

-- | A composite operation for motion and scattering
translateAndScatterRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndScatterRP particle distance = (event, particle') where 
  motion    = displacement (rpDir particle) distance
  particle' = scatterRP $ translateRP particle distance
  momentum  = Momentum ((dir $ rpDir particle') - (dir $ rpDir particle))
  event     = Event motion (Scatter momentum)

-- | A composite event for motion and termination
translateAndTerminateRP :: RandomParticle -> Distance -> (Event, RandomParticle)
translateAndTerminateRP particle distance = (event, Dead) where
  motion    = displacement (rpDir particle) distance
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


