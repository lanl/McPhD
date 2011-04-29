{-| Opacity values: merger of microscopic cross section and macroscopic 
 -  material properties -}

module Opacity where

import qualified Sigma_HBFC as Sigma
import Cell
import Material
import Physical

-- | Total collision opacity
op_collide :: Cell -> Energy -> Sigma.Lepton -> Opacity
op_collide cell e_nu sigLep =
  (op_N cell e_nu) + (op_lepton cell e_nu sigLep)

-- * nucleon opacities
-- | Total nucleon opacity
op_N :: Cell -> Energy -> Opacity
op_N c e = (op_N_abs c e) + (op_N_elastic c e)

-- | nucleon absorption
op_N_abs :: Cell -> Energy -> Opacity
op_N_abs c e_nu = mkOp (rhoN c) (Sigma.nu_N_abs e_nu)

-- | nucleon elastic scattering
op_N_elastic :: Cell -> Energy -> Opacity
op_N_elastic c e_nu = mkOp (rhoN c) (Sigma.nu_N_elastic e_nu)

-- * lepton opacities: these are complicated by the need to sample the
-- lepton energy, as well as differentiation of reaction by 
-- neutrino species. We solve the latter problem by asking the caller
-- to provide functions for the microscopic cross-sections.

-- | total lepton opacity, using mean matter temperature as measure of 
-- lepton energy
op_lepton :: Cell -> Energy -> Sigma.Lepton -> Opacity
op_lepton c e s = (op_e_minus c e s) + (op_e_plus c e s)
  
-- | electron opacity
op_e_minus :: Cell -> Energy -> Sigma.Lepton -> Opacity
op_e_minus cell e_nu sigmas = mkOp rho_e sigma
  where rho_e = rho_e_minus $ mat cell
        sigma = (Sigma.e_minus sigmas) e_nu (tToEnergy cell)

-- | positron opacity
op_e_plus :: Cell -> Energy -> Sigma.Lepton -> Opacity
op_e_plus cell e_nu sigmas = mkOp rho_p sigma
  where rho_p = rho_e_plus $ mat cell
        sigma = (Sigma.e_plus sigmas) e_nu (tToEnergy cell)

-- economize some of the unwrapping -> wrapping
mkOp :: Density -> CrossSection -> Opacity
mkOp r s = Opacity $ (rho r) * (mu s)

tToEnergy :: Cell -> Energy
tToEnergy c = Energy . temp . tempE $ mat c

rhoN :: Cell -> Density
rhoN = rho_nucl . mat 

-- end of file
