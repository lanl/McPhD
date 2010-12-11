module Particle.Limited where

import Space
import Particle.Classes

-- A Simple particle, with a limiting distance (e.g. timestep)

data LimitedParticle = LimitedParticle { l_p :: Position, l_d :: Direction, l_l :: Distance } deriving Show
instance InSpace LimitedParticle where
  position  = l_p
  direction = l_d
  move particle distance = let
    limit = l_l particle
    (limit', distance')      = limiter limit distance
    (position', direction')  = translate_p particle distance'
    in LimitedParticle position' direction' limit' 


limiter :: Distance -> Distance -> (Distance, Distance)
limiter (Distance limit) (Distance distance) 
  | distance < limit = (Distance $ limit-distance, Distance $ distance)
  | otherwise        = (Distance 0, Distance limit)