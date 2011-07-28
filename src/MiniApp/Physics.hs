module MiniApp.Physics where

import Coordinate.Classes

import NormalizedValues
import Properties

import MiniApp.Particle

-- | Properties of the material. This is specific to the MiniApp
data (Space s) => Data s = Data {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
    }


-- | Define momentum
newtype (Space s) => Momentum s = Momentum { momValue :: Scaled (Direction s) }

-- | This is more of a utitlity function, since other models could use
-- it as well.
isotropicScatter :: Particle m -> Particle m
isotropicScatter = undefined
