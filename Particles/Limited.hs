{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Particles.Limited where

import Particles.Classes
import Particles.Data

-- A Simple particle, with a limiting distance (e.g. timestep)

data LimitedParticle = LimitedParticle { l_p :: Position, l_d :: Direction, l_l :: Limit } deriving Show
instance InSpace LimitedParticle where
  position  = l_p
  direction = l_d
  
instance Limited LimitedParticle where
  value = l_l
  actual particle distance
    | distance < limit = (limit - distance, distance)
    | otherwise        = (0, limit)
    where limit = value particle

instance Advance LimitedParticle Double where
  advance particle distance 
   | (value particle) <= 0 = particle
   | otherwise             = LimitedParticle (move particle distance) (direction particle) remaining
   where (remaining, _) = actual particle distance
