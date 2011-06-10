module MiniApp.Physics where

import Space.Classes

import Properties

import MiniApp.Particle

-- | Properties of the material. This is specific to the MiniApp
data (Space s) => Physics s = Physics {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
    }

-- | This is more of a utitlity function, since other models could use
-- it as well.
isotropicScatter :: Particle m -> Particle m
isotropicScatter = undefined
