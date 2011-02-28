module Tallies where

import Particle.Classes

tally :: (Tally t, Event env) => t -> [env] -> t
tally = undefined
