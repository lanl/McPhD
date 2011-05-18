module Particle where

import Physical
import PRNG

data Particle = Particle {
    pos     :: !Position
  , dir     :: !Direction
  , time    :: !Time
  , energy  :: !Energy
  , weight  :: !EnergyWeight
  , cellIdx    :: !CellIdx
  , rng     :: !RNG
  }

-- end of file

