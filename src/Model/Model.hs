module Model.Model where

import Event.Events
import Particle.ParametricParticle

data Ix idx => Model idx = Model { opacity :: IArray idx Double } 

