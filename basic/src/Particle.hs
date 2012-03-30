module Particle where

import Physical

data Particle = Particle {
    pos     :: !Position
  , dir     :: !Direction
  , time    :: !Time
  , energy  :: !Energy
  , weight  :: !EnergyWeight
  , cellIdx :: !CellIdx
  } deriving Show

-- may want to expand on NuX in the future
data PType = NuE | NuEBar | NuX deriving Show

-- end of file

