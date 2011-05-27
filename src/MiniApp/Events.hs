{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}


{-| Datatypes for events generated during simulation.

Limiters of particle motion fall into categories:

 - Collisions: Consist of various kinds of physical scattering and absoroption

 - Boundaries: Geometric interfaces, e.g. cells, mesh boundaries.

 - Census: A category of one. Represents reaching the end of the timestep.

-}
module MiniApp.Events where

import Space.Classes
import Mesh.Classes

-- | Type of Scattering Limiters
data CollideType = Scatter | Absorb  deriving (Eq, Show, Ord)  -- More kinds to come.

-- | Type of Boundary Limiters
data BoundaryType = Cell | Escape | Reflect deriving (Eq, Show, Ord)

-- | Combining the limiters into a single data type with tally information.
data (Mesh m) => Limiter m = Collide  { collideType   :: CollideType
                                      , deltaMomentum :: Momentum (MeshSpace m)
                                      , energyDep     :: Double -- For now
                                      }
                           | Boundary { boundaryType :: BoundaryType
                                      , faceIndex    :: MeshFace m
                                      }
                           | Census   { deltaMomentum :: Momentum (MeshSpace m)
                                      }
deriving instance (Mesh m, Show m
                  , Show (MeshSpace m)
                  , Show (MeshFace m)
                  , Show (Momentum (MeshSpace m))) => Show (Limiter m)

-- | An Event is motion, and a limiter.
data (Mesh m) => Event m = Event { 
      eventMotion  :: Motion (MeshSpace m)
    , eventLimiter :: Limiter m 
    }
deriving instance (Mesh m
                  , Show (Limiter m)
                  , Show (Motion (MeshSpace m))) => Show (Event m)

-- | Strip the motion and just test the limiter.
isFinalEvent :: (Mesh m) => Event m -> Bool
isFinalEvent = isFinalLimiter . eventLimiter

-- | Returns True if a step limiter stops the particle streaming.
isFinalLimiter :: (Mesh m) => Limiter m -> Bool
isFinalLimiter (Collide Absorb _ _) = True
isFinalLimiter (Boundary Escape _)  = True
isFinalLimiter (Census _)           = True
isFinalLimiter _                    = False

