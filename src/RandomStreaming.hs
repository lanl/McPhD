-- | A module to implement a minimal basic streaming for a partile
-- which undergoes random scattering and absorption.

module RandomStreaming where

import Space3DCartesian
import Particle.RandomParticle
import RandomValues

import Data.List


step :: Opacity         -- ^ Opacity in this environment
     -> Time            -- ^ Time limit on streaming
     -> RandomParticle  -- ^ A particle to step
     -> Maybe (Event, RandomParticle)  -- ^ The new particle, and what happened to it. Nothing means we're done.
step _ _ Dead = Nothing
step opacity time particle =
  let (scatter_distance, randState) =
	randomExponential (1.0) (rpRand particle)
  in stepRP opacity time scatter_distance particle{rpRand = randState}


-- * Simulation Functions

stream :: Opacity             -- ^ A single opacity for the stream.
          -> Time             -- ^ Time limit for the particle to stream
          -> RandomParticle   -- ^ A particle to stream
          -> [Event]          -- ^ List of resulting events.
stream opacity time initial_particle = unfoldr (step opacity time) initial_particle
