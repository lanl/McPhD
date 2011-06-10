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
  where (ecom,_) = labToComoving e o v
        v = (mvel $ mat c)


-- | Distance to generic collision: the lesser of -log(\xi/\simga_t) and huge
-- where xi is a uniform random deviate [0,1) and sigma_t is the total 
-- collision opacity. Assume energy in comoving frame. 
-- Note that distance is in the lab frame!
dCollideComoving :: Cell -> 
                    Energy ->
                    Sigma.Lepton -> URD -> Distance
dCollideComoving c e sig (URD xi) = Distance $ min (- (log xi) / sig_c) huge
  where sig_c = mu $ opCollide c e sig


-- | sampleCollision: 
-- (1) transform to comoving frame,  
-- (2) use sampleCollisionComoving to get collision type, new
--     energy, and new direction,
-- (3) transform new energy and momentum back to the lab frame,
-- (4) record energy and momentum deposition in the lab frame.
sampleCollision :: Mesh m => m -> Cell -> Energy -> Direction -> 
                   Sigma.Lepton -> Distance -> EnergyWeight ->
                   Rnd (E.Event,Direction)
sampleCollision m cll 
                elin@(Energy eli) olin@(Direction oli)
                sig dCol (EnergyWeight wt) = do
  let v       = (mvel $ mat cll)
      (eci,_) = labToComoving elin olin v
  (collType,ecf,ocf) <- sampleCollisionComoving m cll eci sig 
  let (Energy elf, ol'@(Direction olf)) = comovingToLab ecf ocf v
      edep = Energy $ wt * (eli - elf)
      pdep = Momentum $ wt/c*(eli*oli - elf*olf) 
  return (E.Collision collType dCol pdep edep,ol')


-- | sample a collision event in the comoving frame: Given the cell, 
-- neutrino energy (comoving), and a uniform random deviate, select
-- a scattering event. Determine final energy and momentum: at present,
-- scattering is elastic and isotropic in comoving frame.
sampleCollisionComoving :: Mesh m => m -> Cell -> Energy -> Sigma.Lepton -> 
                          Rnd (E.CollType,Energy,Direction)
sampleCollisionComoving m c e sig = do
  xi <- random
  ocf <- sampleDirectionIso m
  let ecf = e
      evt = selectEvent c e sig (URD xi)
  return (evt, ecf, ocf)


-- | sample a collision event in the comoving frame: Given the cell, 
-- neutrino energy (comoving), and a uniform random deviate, select
-- a scattering event. Assign distance, momentum, and energy weight
-- to that event. 
selectEvent :: Cell -> Energy -> Sigma.Lepton -> URD -> E.CollType
selectEvent c e sig (URD xi) 
  -- the order here must match the order in eventProbs
  -- (that in turn comes from opacities).
  | xi < pNAbs     = E.NuclAbs 
  | xi < pNElastic = E.NuclEl 
  | xi < pEMinus   = E.EMinusInel
  | xi < pEPlus    = E.EPlusInel 
  | otherwise      = error $ "Unable to assign event, xi = " ++ show xi ++ 
                     ", max event prob = " ++ show pEPlus
  where [pNAbs, pNElastic, pEMinus, pEPlus] = eventProbs c e sig


-- | Cumulative probabilities of scattering events.  
eventProbs :: Cell -> Energy -> Sigma.Lepton -> [FP]
eventProbs c e sig = scanl1 (+) $ map (/ sum ops) ops -- p_i = op_i/(sum_j op_j)
  where ops = mu <$> opacities c e sig
