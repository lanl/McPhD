module Material where

import Physical

data Material = Material {
    sig_abs     :: !Opacity
  , sig_scat    :: !Opacity
  , vel         :: !Velocity    -- cm/sec
  , tempE       :: !Temperature -- in energy units (MeV)
  , rho_nucl    :: !Density     -- g/cc
  , rho_e_minus :: !Density
  , rho_e_plus  :: !Density
  } deriving (Show, Eq)
