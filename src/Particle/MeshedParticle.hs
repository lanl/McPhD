module Particle.MeshedParticle where

import Utils.Finders
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

particleEscape :: MeshedParticle -> (MeshedParticle, MeshedLimiter)
particleEscape = undefined

particleFaceCross :: MeshedParticle -> (MeshedParticle, MeshedLimiter)
particleFaceCross = undefined

particleScatter :: MeshedParticle -> (MeshedParticle, MeshedLimiter)
particleScatter = undefined

particleEndStep :: MeshedParticle -> (MeshedParticle, MeshedLimiter)
particleEndStep = undefined

-- | An association list pairing 

-- step :: Opacity -> Time -> MeshedParticle -> Maybe (Event, MeshedParticle)
-- step opacity final_time meshed_particle =
--   let particle = getParticle meshed_particle 
--       dist_to_final_time      = distanceToTime (final_time - rpTime particle) (rpSpeed particle)
--       dist_to_scatter         = Distance $ (dis . rpMFPToScatter particle) * (opValue . opacity)
--       dist_to_face            = Distance 1.0e6
--       (which_event, distance) = mindex [dist_to_final_time, dist_to_scatter, dist_to_face]
--       (motion, particle')     = step' distance particle
