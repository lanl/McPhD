-- | A module to stream the basic random particle on the simple mesh

module MeshStreaming where

import Particle.MeshedParticle
import Particle.RandomParticle
import Mesh.SimpleCartesian
import RandomValues

data P = MParticle CellIndex RandomParticle

data Opacity = Opacity

step :: Opacity -> SimpleMesh -> P -> Maybe (Event, P)
step = undefined

stream :: Opacity -> SimpleMesh -> P -> [Event]
stream = undefined