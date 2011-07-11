-- TallyIM.hs: Tally implemented with IntMap
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns #-}

module TallyIM ( Tally(..)
               , EventCount(..)
               , CellTally(..)
               , tally
               , merge
               )
  where

import Physical
import Particle
import Event
import Mesh

import Data.List
import qualified Data.IntMap as Map 
import Control.DeepSeq
import Control.Monad
import Data.Monoid
-- | Should and will be in Data.Monoid soon.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
{-# INLINE (<>) #-}


data Tally = Tally { globalEvts  :: !EventCount 
                   , deposition  :: !PhysicsTally
                   , escape      :: ![(Energy,EnergyWeight)]} deriving Show

instance NFData Tally

data CellTally     = CellTally {ctMom :: !Momentum, ctEnergy :: !Energy}
                     deriving (Show,Eq)

type PhysicsTally  = Map.IntMap CellTally

type EscapeCount = [(Energy,EnergyWeight)]

data EventCount = EventCount {
    nNuclAbs    :: !Int
  , nNuclEl     :: !Int
  , nEMinusInel :: !Int
  , nEPlusInel  :: !Int
  , nTransmit   :: !Int
  , nReflect    :: !Int
  , nEscape     :: !Int
  , nTimeout    :: !Int
  } deriving (Show, Eq)


merge :: Tally -> Tally -> Tally
merge (Tally ec1 dep1 esc1) (Tally ec2 dep2 esc2) =
  Tally (ec1 <> ec2) (Map.unionWith (<>) dep1 dep2) (esc1 ++ esc2)

tally :: Mesh m => m -> [(Event,Particle)] -> Tally
tally _ = foldl' tallyImpl emptyTally

tallyImpl :: Tally -> (Event,Particle) -> Tally 
tallyImpl (Tally ec dep esc) (evt,p)  = 
  Tally (countEvent evt ec) (tDep evt (cellIdx p) dep) (tEsc evt esc)

tDep :: Event -> CellIdx -> PhysicsTally -> PhysicsTally
tDep (Collision _ _ dp ed) (CellIdx cidx) tlly = 
  Map.insertWith (<>) cidx (CellTally dp ed) tlly
tDep _ _ t    = t

-- tally an Escape event
tEsc :: Event -> EscapeCount -> EscapeCount
tEsc (Boundary Escape _ _ ed wt) ec = (ed,wt):ec
tEsc _ ec = ec

-- | Count a single event.
countEvent :: Event -> EventCount -> EventCount
countEvent (Collision {cType = NuclEl})     ctr = ctr { nNuclEl = 1 + nNuclEl ctr}
countEvent (Collision {cType = NuclAbs})    ctr = ctr { nNuclAbs = 1 + nNuclAbs ctr}
countEvent (Collision {cType = EMinusInel}) ctr = ctr { nEMinusInel = 1 + nEMinusInel ctr}
countEvent (Collision {cType = EPlusInel})  ctr = ctr { nEPlusInel = 1 + nEPlusInel ctr}
countEvent (Boundary  {bType = Escape})   ctr = ctr { nEscape   = 1 + nEscape   ctr}
countEvent (Boundary  {bType = Reflect})  ctr = ctr { nReflect  = 1 + nReflect  ctr}
countEvent (Boundary  {bType = Transmit}) ctr = ctr { nTransmit  = 1 + nTransmit  ctr}
countEvent (Timeout   {}) ctr = ctr { nTimeout   = 1 + nTimeout   ctr}

instance Monoid EventCount where
  mempty = EventCount 0 0 0 0 0 0 0 0
  mappend (EventCount na1 ne1 emi1 epi1 t1 r1 e1 c1) (EventCount na2 ne2 emi2 epi2 t2 r2 e2 c2) =
    EventCount (na1 + na2) (ne1 + ne2) (emi1 + emi2) (epi1 + epi2) (t1 + t2) (r1 + r2) (e1 + e2) (c1 + c2)

instance Monoid CellTally where
  mempty = CellTally 0 0
  mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

emptyTally :: Tally
emptyTally = Tally mempty Map.empty mempty

-- version
-- $Id$

-- End of file


