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
  } deriving Show

-- may want to expand on NuX in the future
data PType = NuE | NuEBar | NuX deriving Show

-- end of file

