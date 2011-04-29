module Particle where

import Physical
import PRNG

data Particle = Particle {
    pos     :: !Position
  , dir     :: !Direction
  , time    :: !Time
  , energy  :: !Energy
  , weight  :: !EnergyWeight
  , cell    :: !CellIdx
  , rng     :: !RNG
  }

-- end of file

