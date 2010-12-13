{-# OPTIONS_GHC -XTypeFamilies #-}

import Particle.Null
import Mesh.Null
import Events.Event
import Streaming


data NullStream = NS { particle :: NullParticle, mesh :: NullMesh }

instance Stream NullStream where
    type Particle = NullParticle
    stream s = Just (NullEvent, s)

nullstream = let p = NullParticle
                 m = NullMesh
             in makeStream (NS p m)


main =  putStrLn $ show (take 10 nullstream)


