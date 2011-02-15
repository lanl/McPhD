-- Event.hs
-- T. M. Kelley
-- Feb 09, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Event (Event(..)) 
    where     

import Physical
import Cell

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
data Event = Scatter  { dist   :: FP          -- distance travelled
                      , deltaP :: Momentum }  -- momentum transfer  
           | Absorb   { dist   :: FP 
                      , deltaP :: Momentum }  
           | Census   { dist   :: FP 
                      , deltaP :: Momentum }
           | Transmit { dist   :: FP 
                      , deltaP :: Momentum
                      , face   :: Face }
           | Escape   { dist   :: FP 
                      , deltaP :: Momentum
                      , face   :: Face }
           | Reflect  { dist   :: FP 
                      , deltaP :: Momentum
                      , face   :: Face }
             deriving (Show,Eq)

-- version
-- $Id$

-- End of file
