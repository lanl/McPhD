module Material where

import Physical

data Material = Material {
    sig_abs  :: !Opacity
  , sig_scat :: !Opacity
  , vel      :: !Velocity
  , temp     :: !Temperature
  } deriving (Show, Eq)
