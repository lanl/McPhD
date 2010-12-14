{-# OPTIONS_GHC -XTypeFamilies #-}

import Particle.Null
import Mesh.Null
import Events.Event
import Stream


data NullStream = NS { particle :: NullParticle, mesh :: NullMesh }

instance Steppable NullStream where
    type Particle = NullParticle
    step s = Just (NullEvent, s)

nullstream = let p = NullParticle
                 m = NullMesh
             in makeStream (NS p m)


main =  putStrLn $ show (take 10 nullstream)


