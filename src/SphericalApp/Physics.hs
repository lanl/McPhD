module SphericalApp.Physics where

import Space.Classes

import NormalizedValues
import Properties

import SphericalApp.Particle

-- | Properties of the material. This is specific to the SphericalApp
data Data = Data { sig_abs   :: !Opacity
                 , sig_scat  :: !Opacity
                 }



