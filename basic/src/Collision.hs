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
dCollide c e o sig xi = dCollideComoving c ecom sig xi
  where (ecom,_) = labToComoving e o (Material.vel $ mat c)


-- | Distance to generic collision: the lesser of -log(\xi/\simga_t) and huge
-- where xi is a uniform random deviate [0,1) and sigma_t is the total 
-- collision opacity. Assume energy in comoving frame. 
-- Note that distance is in the lab frame!
dCollideComoving :: Cell -> 
                    Energy ->
                    Sigma.Lepton -> URD -> Distance
dCollideComoving c e sig (URD xi) = Distance $ min (- (log xi) / sig_c) huge
  where sig_c = mu $ opCollide c e sig


-- | sampleCollision: transform to comoving, then use sampleCollisionComoving
sampleCollision :: Cell -> Energy -> Direction -> Sigma.Lepton ->
                   URD -> Distance -> Momentum -> Energy -> E.Event
sampleCollision c e o sig xi  = sampleCollisionComoving c ecom sig xi
  where (ecom,_) = labToComoving e o (Material.vel $ mat c)


-- | sample a collision event in the comoving frame: Given the cell, 
-- neutrino energy (comoving), and a uniform random deviate, select
-- a scattering event. Assign distance, momentum, and energy weight
-- to that event. 
sampleCollisionComoving :: Cell -> Energy -> Sigma.Lepton -> URD -> 
                           Distance -> Momentum -> Energy -> E.Event
sampleCollisionComoving c e sig (URD xi) 
  | xi < pNAbs     = E.Collision E.NuclAbs     -- the order here must match the order 
  | xi < pNElastic = E.Collision E.NuclEl      -- in eventProbs (ultimately that comes
  | xi < pEMinus   = E.Collision E.EMinusInel  -- from opacities).
  | xi < pEPlus    = E.Collision E.EPlusInel
  | otherwise      = error $ "Unable to assign event, xi = " ++ show xi ++ 
                     ", max event prob = " ++ show pEPlus
  where [pNAbs, pNElastic, pEMinus, pEPlus] = eventProbs c e sig



-- | sample direction isotropically in the comoving frame. This wraps a 
-- call to Mesh.sampleDirection in the appropriate Lorentz transforms. 
sampleDirectionIso :: Mesh m => m -> Cell -> 
                      Energy ->      -- ^ lab frame
                      Direction ->   -- ^ lab frame
                      Rnd Direction  -- ^ new direction, lab frame
sampleDirectionIso m c e o = do
  let  (ecom,_)  = labToComoving e o v
       v         = (Material.vel $ mat c)
  ocom' <- sampleDirection m
  return (snd $ comovingToLab ecom ocom' v)

-- | Compute probabilities of scattering events, arranged as a discrete cumulative
-- distribution.  
eventProbs :: Cell -> Energy -> Sigma.Lepton -> [FP]
eventProbs c e sig = scanl1 (+) $ map (/ sum ops) ops -- p_i = op_i/(sum_j op_j)
  where ops = mu <$> opacities c e sig
