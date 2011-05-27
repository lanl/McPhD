{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}


{-| MiniApp Model

Provide a function which returns an event arising from the interaction
of the particle with the material at each step.


-}
module MiniApp.Model where

import Data.Array.IArray

import Particle.ParametricParticle
import Mesh.Classes
import Space.Classes
import Properties
import Numerics (huge)

import MiniApp.Events

-- | Aliases
type P = ParticleInMesh

-- | Properties of the space.
data (Space s) => Physics s = Physics {
      sig_abs   :: !Opacity
    , sig_scat  :: !Opacity
    , mvel      :: !(Velocity s)
    , tempE     :: !Temperature
    , rhoNucl   :: !Density
    , rhoEMinus :: !NDensity
    , rhoEPlis  :: !NDensity
    }
               
deriving instance (Space s, Eq   (Velocity s)) => Eq   (Physics s)
deriving instance (Space s, Show (Velocity s)) => Show (Physics s) 

-- | The physical model. Consists of a mesh, space properties indexed
-- by mesh cell and the end of time-step
data (Mesh m) => Model m = Model { 
      mesh    :: m
    , physics :: Array (MeshCell m) (Physics (MeshSpace m)) 
    , t_final :: Time 
    }

timeStepEnd_dist :: (Mesh m) => Model m -> P m -> (Event m, P m)
timeStepEnd_dist = undefined


scatter_dist :: (Mesh m) => Model m -> P m -> (Event m, P m)
scatter_dist = undefined

boundary_dist :: (Mesh m) => Model m -> P m -> (Event m, P m)
boundary_dist = undefined

step :: (Mesh m) => m -> Model m -> ParticleInMesh m -> (Event m, ParticleInMesh m)
step = undefined

