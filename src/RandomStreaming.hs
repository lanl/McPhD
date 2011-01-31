-- | A module to implement a minimal basic streaming for a partile
-- which undergoes random scattering and absorption.

module RandomStreaming where

import Space3DCartesian
import Particle.RandomParticle
import RandomValues

import Data.List


step :: Opacity -> Time -> RandomParticle -> Maybe (Event, RandomParticle)
step _ _ Dead = Nothing
step opacity time particle =
  let (scatter_distance, randState) =
	randomExponential (opValue opacity) (rpRand particle)
  in stepRP opacity time scatter_distance particle{rpRand = randState}


-- * Simulation Functions

stream :: Opacity -> Time -> RandomParticle -> [Event]
stream opacity time initial_particle = unfoldr (step opacity time) initial_particle
