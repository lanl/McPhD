-- Particle.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

-- definition of a particle

module Particle ( Particle(..))
  where

import Physical
import PRNG

data Particle = Particle {
    pPos    :: !Position
  , pDir    :: !Direction
  , pTime   :: !Time
  , pEnergy :: !Energy
  , pWeight :: !EnergyWeight
  , pCell   :: !CellIdx
  , pRNG    :: !RNG
  , pTag    :: !Tag
  } deriving (Show)
             


-- version
-- $Id$

-- End of file
