-- Particle.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

-- definition of a particle

module Particle ( Particle(..))
    where

-- import System.Random.Mersenne.Pure64
-- import Data.Word
-- import System.Random
import Physical
-- import Numerical

data Particle = Particle {
      pPos :: Position
    , pDir :: Direction
    , pTime :: Time
    , pEnergy :: Energy
    , pWeight :: EnergyWeight
    , pCell :: CellIdx
    , pRNG :: RNG
    , pTag :: Tag
    } 

instance Show Particle where
    show p = "Particle: r = " ++ (show.v1x.pos.pPos) p
             ++ ", Omega_r = " ++ (show.v1x.dir.pDir) p 
             ++ ", t = " ++ (show.t.pTime) p
             ++ ", e = " ++ (show.e.pEnergy) p
             ++ ", ew = " ++ (show.ew.pWeight) p
             ++ ", cell = " ++ (show.idx.pCell) p
             ++ ", tag = " ++ (show.pTag) p

-- version
-- $Id$

-- End of file
