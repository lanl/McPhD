-- | A module to implement a minimal basic streaming for a partile
-- which undergoes random scattering and absorption.

module RandomStreaming where

import Particle.RandomParticle
import RandomValues

import Data.List


step :: Opacity -> RandomParticle -> Maybe (Event, RandomParticle)
step _ Dead = Nothing
step opacity particle =
  let (scatter_distance, randState) =
	randomExponential (opValue opacity) (rpRand particle)
  in stepRP opacity scatter_distance particle{rpRand = randState}


-- * Simulation Functions

stream :: Opacity -> RandomParticle -> [Event]
stream opacity initial_particle = unfoldr (step opacity) initial_particle
