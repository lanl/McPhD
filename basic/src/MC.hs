-- MC.hs
-- T. M. Kelley
-- Dec 06, 2010
-- Copyright (c) 2010 LANSLLC all rights reserved.

module MC (runParticle, pickEvent)  -- Added pickEvent here for testing only. Is there a better way?
    where

import Physical
import Particle
import PRNG
import Material
import Mesh
import Tally
import Event
-- import Data.Function
import Data.List (minimumBy)
import Data.Ord (comparing)

runParticle :: Mesh -> Material -> Particle -> Tally
runParticle mesh matl p = tally_ $ push p mesh matl

{- Use a particle (and its RNG), a mesh, and a material to
 - generate a list of (Event, Particle) pairs. -}
push :: Particle -> Mesh -> Material -> [(Event,Particle)]
push p msh matl =  
  let (sel_s,sel_a,ds1,ds2,ds3,rng') = getFiveRNs $ pRNG p
      omega' = sampleDirection msh ds1 ds2 ds3
      evt    = pickEvent p sel_s sel_a omega' matl msh
      p'     = stream p evt msh omega' rng' in
  if isContinuing evt
    then (evt,p'): push p' msh matl
    else [(evt,p')]

-- Pick event for a particle. Provide particle, two uniform random deviates
-- (to select distances), a new direction (however sampled), a material, and a 
-- mesh; get an Event in return. Distance to event and momentum transfer are 
-- encoded in the event. For boundary events, the face is also encoded.
pickEvent :: Particle -> 
             FP   ->    -- distance to scatter selector (RN)
             FP   ->    -- distance to absorption selector (RN)
             Direction ->  -- omega': sampled new direction from scatter (lab frame) 
             Material -> 
             Mesh -> 
             Event
pickEvent p sel_s sel_a omega' matl msh = 
    let (d_bdy,face)  = distToBdy msh cell x omega
        d_scat = min (-log( sel_s)/sig_s) huge -- knows div-by-0 > than huge...
        d_abs  = min (-log( sel_a)/sig_a) huge -- ...but what about sigfpe?
        d_cen  = c * tcen where tcen = t $ pTime p
        cellt  = bdyEvent msh cell face
    in closestEvent [Scatter d_scat (dp omega') (pWeight p), 
                     Absorb  d_abs (dp 0) (pWeight p), -- omega' is 0 
                     cellt   d_bdy face,
                     Census  d_cen 0]
        where sig_s = sigma $ sig_scat ( mat matl ! cell)
              sig_a = sigma $ sig_abs  ( mat matl ! cell)
              x     = pPos p
              cell  = pCell p
              -- TO DO: boost to co-moving frame, compute the scatter,
              -- boost back to lab frame.
              omega = pDir p
              dp    = elasticScatterMomDep (pEnergy p) omega 

-- |Generate a particle at the event with the RNG, and one or more of 
-- new direction, cell, etc 
stream :: Particle -> Event -> Mesh -> Direction -> RNG -> Particle
stream p event msh omega' rng' = 
    case event of
      Scatter {}   -> p' {pDir= omega',    pRNG=rng'}
      Absorb {}    -> p' {pDir= omega,     pRNG=rng'}
      Reflect {}   -> p' {pDir= -omega,  pRNG=rng'}
      Transmit _ f -> p' {pCell=newcell f,pRNG=rng'}
      Escape {}    -> p' {pCell=0,        pRNG=rng'}
      Census {}    -> p' {pTime=0,        pRNG=rng'}
    where p'      = p{pPos = x', pTime = t'}
          d       = dist event
          x'      = pPos p + Position (d *| dir omega)
          t'      = pTime p - Time (d/c)
          omega   = pDir p
          newcell = cellAcross msh (pCell p)

-- must have at least one event!
closestEvent :: [Event] -> Event
closestEvent = minimumBy (comparing dist)

-- | compute the elastic scattering momentum deposition: E/c (k_i^ - k_f)
elasticScatterMomDep :: Energy -> Direction -> Direction -> Momentum
elasticScatterMomDep energy omega_i omega_f = Momentum ((nrg/c) *| dir (omega_i - omega_f))
    where nrg = e energy


-- version
-- $Id$

-- End of file
