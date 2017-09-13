-- TallyIM.hs: Tally implemented with IntMap
-- T. M. Kelley
-- Feb 03, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances,
  DeriveGeneric #-}

module TallyIM ( Tally(..)
               , EventCount(..)
               , CellTally(..)
               , tally
               , emptyTally
               , merge
               , totalDep
               , totalMCSteps
               , PhysicsTally
               , EscapeCount
               )
  where

import Physical
import Particle
import Event
import Mesh

import Control.DeepSeq
import Control.Monad (replicateM, liftM2)
import Data.HashMap.Strict as Map
import Data.List as List
import Data.Monoid (Monoid, mempty, mappend, (<>) )
import Data.Serialize
import GHC.Generics (Generic)

data Tally = Tally { globalEvts  :: !EventCount
                   , deposition  :: !PhysicsTally
                   , escape      :: !EscapeCount
                   , totalPL     :: !Distance -- total path length
                                              -- travelled by all particles
                   } deriving (Eq,Show)

data CellTally     = CellTally {ctMom :: !Momentum, ctEnergy :: !Energy}
                     deriving (Show,Eq,Generic)

type PhysicsTally  = Map.HashMap Int CellTally

type EscapeCount   = [(Energy,EnergyWeight)]

data EventCount    = EventCount {
    nNuclAbs    :: !Int
  , nNuclEl     :: !Int
  , nEMinusInel :: !Int
  , nEPlusInel  :: !Int
  , nTransmit   :: !Int
  , nReflect    :: !Int
  , nEscape     :: !Int
  , nTimeout    :: !Int
  } deriving (Show, Eq, Generic)

tally :: Mesh m => m -> [(Event,Particle)] -> Tally
tally msh = List.foldl' tallyImpl (emptyTally msh)

-- | Tally an event
tallyImpl :: Tally -> (Event,Particle) -> Tally
tallyImpl (Tally ec dep esc pl) (evt,p)  =
  Tally (countEvent evt ec) (tDep evt (cellIdx p) dep) (tEsc evt esc) (tPL evt pl)

-- | Tally momentum deposition.
tDep :: Event -> CellIdx -> PhysicsTally -> PhysicsTally
tDep (Collision _ _
                (Direction oli)
                (Energy ei)
                (Direction olf)
                (Energy ef)
                (EnergyWeight wt)) (CellIdx cidx) tlly =
  Map.insertWith (<>) cidx (CellTally pd ed) tlly
     where ed = Energy $   wt * (ei - ef)
           pd = Momentum $ wt / c * (ei * oli - ef * olf)
tDep _ _ t = t

-- | Tally an Escape event.
tEsc :: Event -> EscapeCount -> EscapeCount
tEsc (Boundary Escape _ _ ed wt) ec = (ed,wt):ec
tEsc _ ec = ec

-- | Tally path length.
tPL :: Event -> Distance -> Distance
tPL evt pl = pl + dist evt

-- | Count an event.
countEvent :: Event -> EventCount -> EventCount
countEvent (Collision {cType = NuclEl})     ctr = ctr { nNuclEl     = 1 + nNuclEl ctr}
countEvent (Collision {cType = NuclAbs})    ctr = ctr { nNuclAbs    = 1 + nNuclAbs ctr}
countEvent (Collision {cType = EMinusInel}) ctr = ctr { nEMinusInel = 1 + nEMinusInel ctr}
countEvent (Collision {cType = EPlusInel})  ctr = ctr { nEPlusInel  = 1 + nEPlusInel ctr}
countEvent (Boundary  {bType = Escape})     ctr = ctr { nEscape     = 1 + nEscape   ctr}
countEvent (Boundary  {bType = Reflect})    ctr = ctr { nReflect    = 1 + nReflect  ctr}
countEvent (Boundary  {bType = Transmit})   ctr = ctr { nTransmit   = 1 + nTransmit  ctr}
countEvent (Timeout   {})                   ctr = ctr { nTimeout    = 1 + nTimeout   ctr}

totalMCSteps :: EventCount -> Int
totalMCSteps (EventCount na ne nem nep nt nr nesc nto) =
  na + ne + nem + nep + nt + nr + nesc + nto

instance Monoid EventCount where
  mempty = EventCount 0 0 0 0 0 0 0 0
  mappend (EventCount na1 ne1 emi1 epi1 t1 r1 e1 c1)
            (EventCount na2 ne2 emi2 epi2 t2 r2 e2 c2) =
              EventCount (na1 + na2) (ne1 + ne2) (emi1 + emi2) (epi1 + epi2)
                           (t1 + t2) (r1 + r2) (e1 + e2) (c1 + c2)

instance Monoid CellTally where
  mempty = CellTally 0 0
  mappend (CellTally m1 e1) (CellTally m2 e2) = CellTally (m1 + m2) (e1 + e2)

instance Monoid Tally where
  mempty  = emptyTally undefined
  mappend = merge

emptyTally :: m -> Tally
emptyTally _ = Tally mempty Map.empty  mempty 0

merge :: Tally -> Tally -> Tally
merge (Tally ec1 dep1 esc1 pl1) (Tally ec2 dep2  esc2 pl2) =
  let r = Tally (ec1 <> ec2) (Map.unionWith (<>) dep1 dep2)  (esc1 ++ esc2) (pl1 + pl2)
  in r `deepseq` r

totalDep :: Tally -> CellTally
totalDep t = Map.foldl' mappend mempty (deposition t)


--                       -----      Serialize instances      -----

instance Serialize EventCount where
  put (EventCount na ne nem nep nx nr nesc nto) = do
                                put na
                                put ne
                                put nem
                                put nep
                                put nx
                                put nr
                                put nesc
                                put nto
  get = do
    na   <- get
    ne   <- get
    nem  <- get
    nep  <- get
    nx   <- get
    nr   <- get
    nesc <- get
    nto  <- get
    return $ EventCount na ne nem nep nx nr nesc nto

instance Serialize CellTally where
  put (CellTally m e) = put m >> put e
  get = do
    m <- get
    e <- get
    return $ CellTally m e

instance Serialize PhysicsTally where
  put pt =
    (put $ size pt) >>
    mapM (\(k,v) -> put k >> put v) (toList pt) >> return ()
  get = do
    sz  <- get
    kvs <- replicateM sz get2
    return $ Map.fromList kvs
    where get2 = liftM2 (,) get get

instance Serialize Tally where
  put (Tally ec dep escs plen) = put ec >> put dep >> put escs >> put plen >> return ()
  get = do
    ec   <- get
    dep  <- get
    escs <- get
    plen <- get
    return $ Tally ec dep escs plen

--                     -----       NFData instances      -----

instance NFData Tally where
  rnf (Tally ge dep  esc dst) =
    ge `deepseq` dep `deepseq`  esc `deepseq` dst `deepseq` ()

instance NFData CellTally

instance NFData EventCount

-- version
-- $Id$

-- End of file


