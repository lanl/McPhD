{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Coordinate.Classes where

import Properties
import Approx ()

class Coordinate s where
  type Position  s :: *
  type Direction s :: *
  type Velocity  s :: *
  stream    :: s -> Distance -> s
  position  :: s -> Position s
  direction :: s -> Direction s
  scale     :: s -> Direction s -> Double -> Velocity s
  make      :: Position s -> Direction s -> s

-- | Infix operator for streaming.
infix 6 +->
(+->) :: (Coordinate s) => s -> Distance -> s
(+->) = stream
