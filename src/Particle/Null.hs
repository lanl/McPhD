{-| Simple "null" particle that is not going anywhere
-}
module Particle.Null where

import Particle.Classes

data NullParticle = NullParticle

instance InSpace NullParticle where
  position  = error "NullParticle has no position"
  direction = error "NullParticle has no direction"
  move = error "NullParticle refuses to move"

