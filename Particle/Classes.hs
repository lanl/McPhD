module Particle.Classes (
  InSpace (..),
  translate_p) where

import Space
import Events.Event

-- Typeclasses
-- -----------

newtype Limit = Limit Double

class InSpace p where
  position  :: p -> Position
  direction :: p -> Direction
  move      :: p -> Distance -> p
  
translate_p :: (InSpace p) => p -> Distance -> (Position, Direction)
translate_p p distance = translate (position p) (direction p) distance




