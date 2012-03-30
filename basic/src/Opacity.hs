{-| Opacity values: merger of microscopic cross section and macroscopic 
 -  material properties. -}

module Opacity (
                module Opacity
               ,module Cell
  ) where

import Sigma_HBFC as Sigma
import Mesh
import Cell
import Material
import Physical

-- | Compute individual opacities in one list.
opacities :: Cell -> Energy -> Sigma.Lepton -> (Opacity,Opacity,Opacity,Opacity)
opacities cll en sig = 
  ( opNAbs cll en
  , opNElastic cll en
  , opEMinus cll en sig
  , opEPlus cll en sig
  )

-- | Total collision opacity
opCollide :: Cell -> Energy -> Sigma.Lepton -> Opacity
-- opCollide cll en sigLep = sum $ opacities cll en sigLep
opCollide cll en sigLep = opA + opE + opEM + opEP
  where (opA,opE,opEM,opEP) = opacities cll en sigLep

-- * nucleon opacities
-- | Total nucleon opacity
opN :: Cell -> Energy -> Opacity
opN cll en = (opNAbs cll en) + (opNElastic cll en)

-- | nucleon absorption
opNAbs :: Cell -> Energy -> Opacity
opNAbs cll en = mkOp (rhoN cll) (Sigma.nuNAbs en)
{-# INLINE opNAbs #-}

-- | nucleon elastic scattering
opNElastic :: Cell -> Energy -> Opacity
opNElastic cll en = mkOp (rhoN cll) (Sigma.nuNElastic en)
{-# INLINE opNElastic #-}

-- * lepton opacities: these are complicated by the need to sample the
-- lepton energy, as well as differentiation of reaction by 
-- neutrino species. We solve the latter problem by asking the caller
-- to provide functions for the microscopic cross-sections.

-- | total lepton opacity, using mean matter temperature as measure of 
-- lepton energy
opLepton :: Cell -> Energy -> Sigma.Lepton -> Opacity
opLepton cll en s = (opEMinus cll en s) + (opEPlus cll en s)
  
-- TO DO: to be consistent with C++ version, we should sample the 
-- the lepton energy

-- | electron opacity
opEMinus :: Cell -> Energy -> Sigma.Lepton -> Opacity
opEMinus cll e_nu sigmas = mkOp rho_e sig
  where rho_e = rhoEMinus $ mat cll
        sig   = (Sigma.e_minus sigmas) e_nu (tToEnergy cll)
        -- sig   = (Sigma.e_minus sigmas) e_nu e_e
        -- e_e   = samplePowerLaw 2.0 (temp . tempE $ mat cll)
{-# INLINE opEMinus #-}

-- | positron opacity
opEPlus :: Cell -> Energy -> Sigma.Lepton -> Opacity
opEPlus cell e_nu sigmas = mkOp rho_p sig
  where rho_p = rhoEPlus $ mat cell
        sig = (Sigma.e_plus sigmas) e_nu (tToEnergy cell)
{-# INLINE opEPlus #-}

-- economize some of the unwrapping -> wrapping
mkOp :: NDensity -> CrossSection -> Opacity
mkOp n s = Opacity $ (nrho n) * (sigma s)
{-# INLINE mkOp #-}

tToEnergy :: Cell -> Energy
tToEnergy cll = Energy . temp . tempE $ mat cll
{-# INLINE tToEnergy #-}

rhoN :: Cell -> NDensity
rhoN = nperCC . rhoNucl . mat 
{-# INLINE rhoN #-}

-- | Convert proton mass density to number density
nperCC :: Density -> NDensity
nperCC (Density r) = NDensity $ r / pmg
{-# INLINE nperCC #-}

-- end of file
