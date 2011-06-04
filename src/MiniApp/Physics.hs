module MiniApp.Physics where

import Space.Classes
import Mesh.Classes

import Properties

import MiniApp.Outcome
import MiniApp.Particle

-- | Properties of the material. This is specific to the MiniApp
data (Space s) => Physics s = Physics {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
    }

-- | Compute an outcome for the particle as it interacts with the material.
physicsOutcome :: Mesh m => Physics (MeshSpace m) -> Particle m -> Outcome m
physicsOutcome = undefined

-- | This is more of a utitlity function. Other physics will have much
-- more complicated scattering models.
isotropicScatter :: Particle m -> Particle m
isotropicScatter = undefined
