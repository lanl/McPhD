-- Tally.hs
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns #-}

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

type PhysicsTally   = Map.Map CellIdx (Momentum,EnergyWeight)

-- trying to find a way to make PhysicsTally strict 
-- newtype PhysicsTally   = PTally {physTally :: Map.Map CellIdx (Momentum,EnergyWeight)} deriving (Show)

data EventCount = EventCount { nScatter  :: !Int 
                             , nAbsorb   :: !Int
                             , nTransmit :: !Int
                             , nReflect  :: !Int
                             , nEscape   :: !Int
                             , nCensus   :: !Int
                             } deriving (Eq)

tally :: Tally -> [(Event,Particle)] -> Tally
tally = foldr tallyImpl 

tally_ :: [(Event,Particle)] -> Tally
tally_ = foldr tallyImpl emptyTally 

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
countEvent Scatter  {} ctr = ctr { nScatter  = 1 + nScatter  ctr}
countEvent Absorb   {} ctr = ctr { nAbsorb   = 1 + nAbsorb   ctr}
countEvent Transmit {} ctr = ctr { nTransmit = 1 + nTransmit ctr}
countEvent Escape   {} ctr = ctr { nEscape   = 1 + nEscape   ctr}
countEvent Reflect  {} ctr = ctr { nReflect  = 1 + nReflect  ctr}
countEvent Census   {} ctr = ctr { nCensus   = 1 + nCensus   ctr}

merge :: Tally -> Tally -> Tally
merge t1 t2 =
    let !newEC = addEventCounts (globalEvts t1) (globalEvts t2)
        !newDep = Map.unionWith plusME (deposition t1) (deposition t2)
    in Tally {globalEvts = newEC,deposition = newDep}

emptyTally :: Tally
emptyTally     = Tally emptyEvtCount emptyPhysTally

emptyPhysTally :: PhysicsTally
emptyPhysTally = Map.empty

emptyEvtCount :: EventCount
emptyEvtCount  = EventCount 0 0 0 0 0 0 

addEventCounts :: EventCount -> EventCount ->EventCount
addEventCounts c1 c2 = EventCount {nScatter = nScatter c1 + nScatter c2
                                  ,nAbsorb = nAbsorb c1 + nAbsorb c2
                                  ,nTransmit = nTransmit c1 + nTransmit c2
                                  ,nReflect = nReflect c1 + nReflect c2
                                  ,nEscape = nEscape c1 + nEscape c2
                                  ,nCensus = nCensus c1 + nCensus c2 }

instance Show EventCount where
  show e = "Event Count"++
           "\n# scat:  " ++ (show $ nScatter e) ++
           "\n# abs:   " ++ (show $ nAbsorb e) ++
           "\n# trans: " ++ (show $ nTransmit e) ++
           "\n# esc:   " ++ (show $ nEscape e) ++
           "\n# refl:  " ++ (show $ nReflect e) ++
           "\n# cen:   " ++ (show $ nCensus e)

-- version
-- $Id$

-- End of file


