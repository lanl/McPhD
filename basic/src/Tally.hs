-- Tally.hs
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Tally (tally
             ,Tally
             ,EventCount
             ,MomentumTally
             ,EnergyTally
             ,emptyMomTally -- exposed only for dev 
             ,emptyEvtCount -- exposed only for dev 
             ,tMom          -- exposed only for dev 
             ,tallyImpl     -- exposed only for dev 
             ,countEvent    -- exposed only for dev 
             )
    where

import qualified Data.Map as Map 
import Numerical
import Physical
import Particle
import Event (Event(..))

data Tally = Tally { globalEvts  :: EventCount 
                   , mDeposition :: MomentumTally 
                   , eDeposition :: EnergyTally } deriving Show


-- want these to be array-based
type MomentumTally  = Map.Map CellIdx Momentum
type EnergyTally    = Map.Map CellIdx EnergyWeight
-- type PhysicsTally   = Map.Map CellIdx (Momentum,Energy)

data EventCount = EventCount { n_scatter  :: !Int -- NOTE: Strict counters are always a good idea
                             , n_absorb   :: !Int
                             , n_transmit :: !Int
                             , n_reflect  :: !Int
                             , n_escape   :: !Int
                             , n_census   :: !Int
                             } deriving Show

-- -- all the random numbers to select an event. Idea was to limit how
-- -- far down IO intrudes into the code.
-- data EventSelectors = EventSelectors {
--       d_sig_s     :: FP   -- sample distance to scatter
--     , d_sig_a     :: FP   -- sample distance to absorb
--     , sel_omega   :: FP   -- sample new direction cosine
--     }


tally :: [(Event,Particle)] -> Tally
tally walk = foldr tallyImpl emptyTally walk

tallyImpl :: (Event,Particle) -> Tally -> Tally 
tallyImpl (e,p) t = Tally (countEvent e eC) (tMom (e,pCell p) mT) (tNrg (e,pCell p) eT)
                    where eC = globalEvts t
                          mT = mDeposition t
                          eT = eDeposition t

tMom :: (Event,CellIdx) -> MomentumTally -> MomentumTally
tMom (Scatter _ dp _,cell) t = Map.insertWith' (+) cell dp t
tMom (Absorb _ dp _,cell)  t = Map.insertWith' (+) cell dp t
tMom _ t    = t

tNrg :: (Event,CellIdx) -> EnergyTally -> EnergyTally
tNrg (Scatter _  _ e,cell) t = Map.insertWith' (+) cell e t
tNrg (Absorb _  _ e,cell)  t = Map.insertWith' (+) cell e t
tNrg _ t    = t

countEvent :: Event -> EventCount -> EventCount
countEvent Scatter {}  ctr = ctr { n_scatter  = 1 + n_scatter  ctr}
countEvent Absorb {}   ctr = ctr { n_absorb   = 1 + n_absorb   ctr}
countEvent Transmit {} ctr = ctr { n_transmit = 1 + n_transmit ctr}
countEvent Escape {}   ctr = ctr { n_escape   = 1 + n_escape   ctr}
countEvent Reflect {}  ctr = ctr { n_reflect  = 1 + n_reflect  ctr}
countEvent Census {}   ctr = ctr { n_census   = 1 + n_census   ctr}

emptyTally :: Tally
emptyTally    = Tally emptyEvtCount emptyMomTally emptyNrgTally 
emptyNrgTally :: EnergyTally
emptyNrgTally = Map.empty :: EnergyTally
emptyMomTally :: MomentumTally
emptyMomTally = Map.empty :: MomentumTally
emptyEvtCount :: EventCount
emptyEvtCount = EventCount 0 0 0 0 0 0 


-- version
-- $Id$

-- End of file