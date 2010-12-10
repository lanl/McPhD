import Particles.Null
import Mesh.Null

import Particles.Classes
import Events.Event

import Data.List
import Control.Monad


data NullStream = NS { particle :: NullParticle, mesh :: NullMesh }

instance Stream NullStream where
    stream s = Just (NullEvent, s)


nullstream = let p = NullParticle
                 m = NullMesh
             in unfoldr stream (NS p m)


main =  putStrLn $ show (take 10 nullstream)