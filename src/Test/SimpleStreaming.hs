import Particle.Simple
import Mesh.Simple
import Events.Event
import Streaming

import Data.Vector.V3
import Data.List

data SimpleStream = SS { particle :: SimpleParticle, mesh :: SimpleMesh, cell :: Cell }

instance Stream SimpleStream where
  stream s = undefined

main = let mesh = SimpleMesh (Size 10 10 10) (Vector3 0.1 0.1 0.1)
       in return ()