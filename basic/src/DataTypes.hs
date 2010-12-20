-- SimpleDataTypes.hs
-- T. M. Kelley
-- Dec 08, 2010
-- (c) Copyright 2010 LANSLLC all rights reserved.

module DataTypes where

import Data.Map

-- types 
type FP       = Double
-- FIX: these should be distinct types
type Momentum = FP  
type CellIdx  = Int 
type Energy   = FP  
type EnergyWeight = FP

-- NOTE: If the range of CellIdx values is contiguous, it is better to use Array here - it will use less space in
-- setting with lots of cells
type Mesh           = Map CellIdx CellProperties
type Material       = Map CellIdx MaterialState
type MomentumTally  = Map CellIdx Momentum
type EnergyTally    = Map CellIdx Energy

data Particle = Particle {
      px      :: FP -- NOTE: you would probably want to make those fields strict
    , pomega  :: FP
    , pt      :: FP 
    , penergy :: Energy         -- dynamic energy of the particle.
    , pweight :: EnergyWeight   -- energy weight: fraction of simulation energy
                                -- that this particle represents.
    , pcell   :: CellIdx
    } deriving Show

data EventCount = EventCount { n_scatter  :: !Int -- NOTE: Strict counters are always a good idea
                             , n_absorb   :: !Int
                             , n_transmit :: !Int
                             , n_reflect  :: !Int
                             , n_escape   :: !Int
                             , n_census   :: !Int
                             } deriving Show
                
{- Events: what can happen to a particle on a Monte Carlo step.
 - Scatter:  some type of scatter.  NB neutrinos have many more interaction 
 -           channels than photons: looks like ~6-8 per lepton, x2 for anti-nu's
 - Absorb:   physically a subset of scattering, computationally different in that
 -           absorption terminates a particle's flight
 - Transmit: reach a cell boundary and cross into a new mesh cell
 - Reflect:  reach a cell boundary and reflect from it
 - Escape:   reach a cell boundary and leave the problem domain
 - Census:   the particle's internal clock has reached the end of the time step;
 -           it is banked for the next time step.
 -
 - I've stuffed the momentum transfer into the Event. Otherwise, we need to 
 - compare successive events or particle states to compute this later.
 -}
data Event = Scatter  { dist   :: FP    -- distance travelled
                      , deltaP :: Momentum }  -- momentum transfer  
           | Absorb   { dist   :: FP 
                      , deltaP :: Momentum }  
           | Transmit { dist   :: FP 
                      , deltaP :: Momentum }
           | Escape   { dist   :: FP 
                      , deltaP :: Momentum }
           | Reflect  { dist   :: FP 
                      , deltaP :: Momentum }
           | Census   { dist   :: FP 
                      , deltaP :: Momentum }
             deriving (Show,Eq)


data BoundaryCondition = Vacuum | Reflective | Transparent

data MaterialState = MatState {
      sigma_s :: FP   -- scattering cross section
    , sigma_a :: FP   -- absorption cross section
    , v       :: FP   -- velocity
    } deriving Show

data CellProperties = CellProps {
      low_x   :: FP
    , high_x  :: FP
    , low_bc  :: BoundaryCondition
    , high_bc :: BoundaryCondition
}

-- all the random numbers to select an event. Idea was to limit how
-- far down IO intrudes into the code.
data EventSelectors = EventSelectors {
      d_sig_s     :: FP   -- sample distance to scatter
    , d_sig_a     :: FP   -- sample distance to absorb
    , sel_omega   :: FP   -- sample new direction cosine
    }



-- version
-- $Id$

-- End of file
