{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A module to implement a minimal basic streaming for a partile
-- which undergoes random scattering and absorption.

module RandomStreaming where

import Space3DCartesian
import Particle.RandomParticle
import RandomValues
import Properties

import Data.List


distanceToTS :: RandomParticle -> Time -> Distance
distanceToTS particle time =
  let time_left = time - rpTime particle
  in distanceToTime time_left (rpSpeed particle)


step :: Opacity         -- ^ Opacity in this environment
     -> Time            -- ^ Time limit on streaming
     -> RandomParticle  -- ^ A particle to step
     -- | The new particle, and what happened to it.
     -> Maybe (RandomEvent, RandomParticle)
step _ _ Dead = Nothing -- ^ Nothing means we're done with this particle.
step opacity time particle =
  let (scatter_distance, randState) =
        randomExponential (opValue opacity) (rpRand particle)
  in stepRP time scatter_distance particle{rpRand = randState}


-- * Simulation Functions

stream :: Opacity             -- ^ A single opacity for the stream.
          -> Time             -- ^ Time limit for the particle to stream
          -> RandomParticle   -- ^ A particle to stream
          -> [RandomEvent]          -- ^ List of resulting events.
stream opacity time initial_particle = unfoldr (step opacity time) initial_particle
