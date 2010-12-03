{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Particles.Classes (
  InSpace (..),
  Limited (..),
  Stream  (..),
  Advance (..)) where

import Particles.Data

-- Typeclasses
-- -----------

class InSpace p where
  position  :: p -> Position
  direction :: p -> Direction
  move      :: p -> Distance -> Position
  move particle distance = daxpy (position particle) (direction particle) distance

class Limited p where
  value    :: p -> Limit
  actual   :: p -> Distance -> (Limit, Distance)

class Advance p where
  advance :: p -> Distance -> p

class Stream p e where
  stream :: p -> Maybe (e, p)
