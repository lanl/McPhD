import Particle.Null
import Mesh.Null
import Events.Event
import Streaming

import Data.List

data NullStream = NS { particle :: NullParticle, mesh :: NullMesh }

instance Stream NullStream where
    stream s = Just (NullEvent, s)


nullstream = let p = NullParticle
                 m = NullMesh
             in unfoldr stream (NS p m)


main =  putStrLn $ show (take 10 nullstream)

