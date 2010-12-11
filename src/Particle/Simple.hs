module Particle.Simple where

import Space
import Particle.Classes

-- A Simple particle.

data SimpleParticle = Simple { s_p :: Position,  s_d :: Direction } deriving Show
instance InSpace SimpleParticle where
  position   = s_p
  direction  = s_d
  move particle distance = 
    Simple position' direction' where
      (position', direction')  = translate_p particle distance
  


