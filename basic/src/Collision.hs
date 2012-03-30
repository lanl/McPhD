{- | Collision sampling functions -}

module Collision (
    module Collision
  , module Opacity
  , module Sigma
  )
  where

import qualified Event as E
import Physical
import Opacity
import Geometry
import Sigma_HBFC as Sigma
import Control.Applicative
import Material
import Mesh
import PRNG

-- | Distance to collision: given energy and direction in the lab frame,
-- compute distance to collision. Note that distance is in the lab frame!
dCollide :: Cell -> Energy -> Direction ->
            Sigma.Lepton -> URD -> Distance
dCollide cll en o sig xi = dCollideComoving cll ecom sig xi
  where (ecom,_) = labToComoving en o v
        v = (mvel $ mat cll)


-- | Distance to generic collision: the lesser of -log(\xi/\simga_t) and huge
-- where xi is a uniform random deviate [0,1) and sigma_t is the total 
-- collision opacity. Assume energy in comoving frame. 
-- Note that distance is in the lab frame!
dCollideComoving :: Cell -> Energy -> Sigma.Lepton -> URD -> Distance
dCollideComoving cll en sig (URD xi) = Distance $ min (- (log xi) / sig_c) huge
  where sig_c = mu $ opCollide cll en sig


-- | sampleCollision: 
-- (1) transform to comoving frame,  
-- (2) use sampleCollisionComoving to get collision type, new
--     energy, and new direction,
-- (3) transform new energy and momentum back to the lab frame,
-- (4) record energy and momentum deposition in the lab frame.
-- Reminder: user should supply the direction at the event site!
sampleCollision :: Mesh m => m -> Cell -> Energy -> Direction -> 
                   Sigma.Lepton -> Distance -> EnergyWeight -> URD -> URD ->
                   E.Event
sampleCollision m cll 
                eli oli
                sig dCol wt xi1 xi2 =
  let v         = (mvel $ mat cll)
      (eci,oci) = labToComoving eli oli v
      (collType,ecf,ocf) = sampleCollisionComoving m cll eci oci sig xi1 xi2
      (elf, olf) = comovingToLab ecf ocf v
  in E.Collision collType dCol oli eli olf elf wt


-- | sample a collision event in the comoving frame: Given the cell, 
-- neutrino energy (comoving), and a uniform random deviate, select
-- a scattering event. Determine final energy and momentum: at present,
-- scattering is elastic and isotropic in comoving frame.
sampleCollisionComoving :: Mesh m => m -> Cell -> Energy -> Direction -> 
                           Sigma.Lepton -> URD -> URD -> (E.CollType,Energy,Direction)
sampleCollisionComoving m cll en oci sig xi1 xi2 =
  let evt = selectEvent cll en sig xi1
      (ecf,ocf) = getNewState evt m cll en oci xi2
  in (evt, ecf, ocf)

getNewState :: Mesh m => E.CollType -> m -> Cell -> Energy -> Direction -> URD ->
               (Energy,Direction)
getNewState E.NuclAbs    _ _ e o _  = Sigma.newStateNAbs e o
getNewState E.NuclEl     m _ e o xi = Sigma.newStateNElastic m xi e o
getNewState E.EMinusInel m c e o xi = Sigma.newStateEMinusInel m xi c e o
getNewState E.EPlusInel  m c e o xi = Sigma.newStateEPlusInel m xi c e o

-- | sample a collision event in the comoving frame: Given the cell, 
-- neutrino energy (comoving), and a uniform random deviate, select
-- a scattering event. Assign distance, momentum, and energy weight
-- to that event. 
selectEvent :: Cell -> Energy -> Sigma.Lepton -> URD -> E.CollType
selectEvent cll en sig (URD xi) 
  -- the order here must match the order in eventProbs
  -- (that in turn comes from opacities).
  | xi < pNAbs     = E.NuclAbs 
  | xi < pNElastic = E.NuclEl 
  | xi < pEMinus   = E.EMinusInel
  | xi < pEPlus    = E.EPlusInel 
  | otherwise      = error $ "Unable to assign event, xi = " ++ show xi ++ 
                     ", max event prob = " ++ show pEPlus
  where (pNAbs, pNElastic, pEMinus, pEPlus) = eventProbs cll en sig


-- | Cumulative probabilities of scattering events.  
eventProbs :: Cell -> Energy -> Sigma.Lepton -> (FP,FP,FP,FP)
eventProbs cll en sig = ( a/norm,
                          (a+el)/norm,
                          (a+el+em)/norm,
                          (a+el+em+ep)/norm) -- p_i = op_i/(sum_j op_j)
  where (Opacity a,Opacity el,Opacity em,Opacity ep) = opacities cll en sig
        norm = a+el+em+ep

{-# INLINE eventProbs #-}

-- end of file
