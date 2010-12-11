import Particles.Null
import Mesh.Null

import Particles.Classes

import Events.Event

data NullStream = NS { particle :: NullParticle, mesh :: NullMesh }

-- data SimpleStream = SS { particle :: SimpleParticle, mesh ::SimpleMesh }

instance Stream NullStream where
    stream s = Just (NullEvent, s)

main = undefined