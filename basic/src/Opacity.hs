{-| Opacity values: merger of microscopic cross section and macroscopic 
 -  material properties. -}

module Opacity where

import qualified Sigma_HBFC as Sigma
import Cell
import Material
import Physical


-- | Compute individual opacities in one list.
opacities :: Cell -> Energy -> Sigma.Lepton -> [Opacity]
opacities c e sig = 
  [ opNAbs c e
  , opNElastic c e
  , opEMinus c e sig
  , opEPlus c e sig
  ]

-- | Total collision opacity
opCollide :: Cell -> Energy -> Sigma.Lepton -> Opacity
opCollide c e sigLep = sum $ opacities c e sigLep

-- * nucleon opacities
-- | Total nucleon opacity
opN :: Cell -> Energy -> Opacity
opN c e = (opNAbs c e) + (opNElastic c e)

-- | nucleon absorption
opNAbs :: Cell -> Energy -> Opacity
opNAbs c e = mkOp (rhoN c) (Sigma.nuNAbs e)

-- | nucleon elastic scattering
opNElastic :: Cell -> Energy -> Opacity
opNElastic c e = mkOp (rhoN c) (Sigma.nuNElastic e)

-- * lepton opacities: these are complicated by the need to sample the
-- lepton energy, as well as differentiation of reaction by 
-- neutrino species. We solve the latter problem by asking the caller
-- to provide functions for the microscopic cross-sections.

-- | total lepton opacity, using mean matter temperature as measure of 
-- lepton energy
opLepton :: Cell -> Energy -> Sigma.Lepton -> Opacity
opLepton c e s = (opEMinus c e s) + (opEPlus c e s)
  
-- | electron opacity
opEMinus :: Cell -> Energy -> Sigma.Lepton -> Opacity
opEMinus cell e_nu sigmas = mkOp rho_e sigma
  where rho_e = rhoEMinus $ mat cell
        sigma = (Sigma.e_minus sigmas) e_nu (tToEnergy cell)

-- | positron opacity
opEPlus :: Cell -> Energy -> Sigma.Lepton -> Opacity
opEPlus cell e_nu sigmas = mkOp rho_p sig
  where rho_p = rhoEPlus $ mat cell
        sig = (Sigma.e_plus sigmas) e_nu (tToEnergy cell)

-- economize some of the unwrapping -> wrapping
mkOp :: Density -> CrossSection -> Opacity
mkOp r s = Opacity $ (rho r) * (sigma s)

tToEnergy :: Cell -> Energy
tToEnergy c = Energy . temp . tempE $ mat c

rhoN :: Cell -> Density
rhoN = rhoNucl . mat 

-- end of file
