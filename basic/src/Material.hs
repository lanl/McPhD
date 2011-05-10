module Material where

import Physical

data Material = Material {
    sig_abs     :: !Opacity
  , sig_scat    :: !Opacity
  , vel         :: !Velocity    -- cm/sec
  , tempE       :: !Temperature -- in energy units (MeV)
  , rhoNucl     :: !Density     -- g/cc
  , rhoEMinus   :: !Density
  , rhoEPlus    :: !Density
  } deriving (Show, Eq)
