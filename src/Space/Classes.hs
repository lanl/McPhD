{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Space.Classes where

import Properties
import Approx ()


class Space s where
  type Position s  :: *
  type Direction s :: *
  stream    :: s -> Distance -> s
  position  :: s -> Position s
  direction :: s -> Direction s

-- | Motion is a combination of a starting location and a distance.
data (Space s) => Motion s = Motion s Distance
deriving instance (Space s, Show s) => Show (Motion s)

-- | Momentum is a weight (energy, mass) and direction.
data (Space s) => Momentum s = Momentum Double (Direction s)

-- | Velocity is a direction and speed. 
data (Space s) => Velocity s = Velocity Speed (Direction s)

-- * Operations on spaces

-- | Infix operator for streaming.
infix 6 +->
(+->) :: (Space s) => s -> Distance -> s
(+->) = stream


