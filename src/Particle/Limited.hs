{-| A Simple particle, with a limiting distance (e.g. timestep)
-}
module Particle.Limited where

import Space
import Particle.Classes

data LimitedParticle = LimitedParticle { l_p :: Position, l_d :: Direction, l_l :: Distance } deriving Show
instance InSpace LimitedParticle where
  position  = l_p
  direction = l_d
  move particle distance = let
    limit = l_l particle
    (limit', distance')      = limiter limit distance
    position'  = translate_p particle distance'
    in particle { l_p=position', l_l=limit' }

-- | Checks whether the distance is withing given limit
limiter :: Distance -- ^ Limit
           -> Distance -- ^ Distance
           -> (Distance, Distance) -- ^ (How much limit is "unused", How much distance fit within limit)
limiter (Distance limit) (Distance distance) 
  | distance < limit = (Distance $ limit-distance, Distance $ distance)
  | otherwise        = (Distance 0, Distance limit)