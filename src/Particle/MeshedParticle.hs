module Particle.MeshedParticle where

import Mesh.SimpleCartesian
import Space3DCartesian

-- | Looking for ways to take a particle and add information about the mesh that it's on

-- | Newtype for a particle, plus an index to keep track of it.
data MeshedParticle i p = MeshedParticle {
      getIndex :: i
    , getParticle :: p
    } deriving (Show)


data MeshedLimiter i p =
    Scatter Momentum
        | Escape (MeshedParticle i p)
        | Termination (MeshedParticle i p)
        | FaceCrossing Face
          deriving (Show)

-- | Functions to process specific limiting events on a particle
