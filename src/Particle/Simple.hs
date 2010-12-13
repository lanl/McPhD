{-| A Simple particle - basically, point in 3d space + direction vector
-}
module Particle.Simple where

import Space
import Particle.Classes

data SimpleParticle = Simple { s_p :: Position,  s_d :: Direction } deriving Show
instance InSpace SimpleParticle where
  position   = s_p
  direction  = s_d
  move particle distance = particle { s_p = position', s_d = direction' }
                           where (position', direction') = translate_p particle distance
  


