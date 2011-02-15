-- Tally.hs
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Tally where


import Data.Map
import Numerical
import Physical

-- want these to be array-based
type MomentumTally  = Map Idx Momentum
type EnergyTally    = Map Idx Energy

data EventCount = EventCount { n_scatter  :: !Int -- NOTE: Strict counters are always a good idea
                             , n_absorb   :: !Int
                             , n_transmit :: !Int
                             , n_reflect  :: !Int
                             , n_escape   :: !Int
                             , n_census   :: !Int
                             } deriving Show

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
