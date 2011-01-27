{-| Generic interface to particles
-}
module Particle.Classes (
  InSpace (..),
  translate_p) where

import Space

-- Typeclasses
-- -----------

-- | Generic moving object in 3d space (think of generic particle)
class InSpace p where
  position  :: p -> Position      -- ^ Get current position
  direction :: p -> Direction     -- ^ Get current direction of movement
  move      :: p -> Distance -> p -- ^ Move the object by given distance
  
-- | Move generic object in space using 'Space.translate'.
translate_p :: (InSpace p) => p -> Distance -> (Position, Direction)
translate_p particle distance = (translate (position particle) (direction particle) distance, (direction particle))

