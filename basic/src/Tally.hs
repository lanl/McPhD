-- Tally.hs
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Tally (tally
             ,tally_
             ,Tally
             ,merge
             ,emptyTally
             -- ,EventCount
             -- ,MomentumTally
             -- ,EnergyTally
             )
    where

import qualified Data.Map as Map 
import Numerical
import Physical
import Particle
import Event (Event(..))

data Tally = Tally { globalEvts  :: EventCount 
                   , deposition  :: PhysicsTally} deriving Show

-- want these to be array-based
type MomentumTally  = Map.Map CellIdx Momentum
type EnergyTally    = Map.Map CellIdx EnergyWeight
type PhysicsTally   = Map.Map CellIdx (Momentum,EnergyWeight)

data EventCount = EventCount { n_scatter  :: !Int -- NOTE: Strict counters are always a good idea
                             , n_absorb   :: !Int
                             , n_transmit :: !Int
                             , n_reflect  :: !Int
                             , n_escape   :: !Int
                             , n_census   :: !Int
                             } deriving (Show,Eq)

tally :: Tally -> [(Event,Particle)] -> Tally
tally t walk = foldr tallyImpl t walk

tally_ :: [(Event,Particle)] -> Tally
tally_ walk = foldr tallyImpl emptyTally walk

tallyImpl :: (Event,Particle) -> Tally -> Tally 
tallyImpl (e,p) t = Tally (countEvent e eC) (tDep (e,pCell p) d) 
                    where eC = globalEvts t
                          d  = deposition t

tDep :: (Event,CellIdx) -> PhysicsTally -> PhysicsTally
tDep (Scatter _ dp e,cell) t = Map.insertWith' plusME cell (dp,e) t
tDep (Absorb _ dp e,cell)  t = Map.insertWith' plusME cell (dp,e) t
tDep _ t    = t

plusME :: (Momentum,EnergyWeight) -> (Momentum,EnergyWeight) -> (Momentum,EnergyWeight)
plusME (dp1,e1) (dp2,e2) = (dp1+dp2,e1+e2) 

countEvent :: Event -> EventCount -> EventCount
countEvent Scatter {}  ctr = ctr { n_scatter  = 1 + n_scatter  ctr}
countEvent Absorb {}   ctr = ctr { n_absorb   = 1 + n_absorb   ctr}
countEvent Transmit {} ctr = ctr { n_transmit = 1 + n_transmit ctr}
countEvent Escape {}   ctr = ctr { n_escape   = 1 + n_escape   ctr}
countEvent Reflect {}  ctr = ctr { n_reflect  = 1 + n_reflect  ctr}
countEvent Census {}   ctr = ctr { n_census   = 1 + n_census   ctr}

merge :: Tally -> Tally -> Tally
merge t1 t2 = Tally {globalEvts = addEventCounts (globalEvts t1) (globalEvts t2)
                    ,deposition = Map.unionWith plusME (deposition t1) (deposition t2)}

emptyTally :: Tally
emptyTally     = Tally emptyEvtCount emptyPhysTally

emptyPhysTally :: PhysicsTally
emptyPhysTally = Map.empty

emptyEvtCount :: EventCount
emptyEvtCount  = EventCount 0 0 0 0 0 0 

addEventCounts :: EventCount -> EventCount ->EventCount
addEventCounts c1 c2 = EventCount {n_scatter = n_scatter c1 + n_scatter c2
                                  ,n_absorb = n_absorb c1 + n_absorb c2
                                  ,n_transmit = n_transmit c1 + n_transmit c2
                                  ,n_reflect = n_reflect c1 + n_reflect c2
                                  ,n_escape = n_escape c1 + n_escape c2
                                  ,n_census = n_census c1 + n_census c2
                                  }

-- version
-- $Id$

-- End of file


