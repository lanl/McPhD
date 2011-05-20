{-# LANGUAGE TypeFamilies, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Space.Classes where

import Approx ()


class Space s where
  type Position s  :: *
  type Direction s :: *
  type Distance s  :: *
  stream    :: s -> Distance s -> s
  position  :: s -> Position s
  direction :: s -> Direction s

-- | Motion is a combination of a starting location and a distance.
data (Space s) => Motion s = Motion s (Distance s)
deriving instance (Space s, Show s,  Show (Distance s)) => Show (Motion s)

-- | Momentum is a weight (energy, mass) and direction.
data (Space s) => Momentum s = Momentum Double (Distance s)

infix 6 +->
(+->) :: (Space s) => s -> Distance s -> s
(+->) = stream

