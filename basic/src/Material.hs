module Material where

import Physical

data Material = Material {
    sig_abs     :: !Opacity
  , sig_scat    :: !Opacity
  , mvel        :: !Velocity    -- cm/sec
  , tempE       :: !Temperature -- in energy units (MeV)
  , rhoNucl     :: !Density     -- g/cc
  , rhoEMinus   :: !NDensity    -- n/cc (derived from electron fraction) 
  , rhoEPlus    :: !NDensity    -- n/cc 
  } deriving (Show, Eq)
