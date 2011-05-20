{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}


{-| Datatypes for events generated during simulation.

Limiters of particle motion fall into categories:

Collisions: Consist of various kinds of physical scattering and absoroption

Boundaries: Geometric interfaces, e.g. cells, mesh boundaries and end of timestep.

Census: A category of one. Represents reaching the end of the timestep.

Questions:
- How should we include paths of motion for things like implicit deposition?

-}
module Events.Event where

import Space.Classes
import Mesh.Classes

-- | Type of Scattering Limiters
data CollideType = Scatter | Absorb  deriving (Eq, Show, Ord)  -- More kinds to come.

-- | Type of Boundary Limiters
data BoundaryType = Cell | Escape | Reflect deriving (Eq, Show, Ord)

-- | Combining the limiters into a single data type with tally information.
data (Space s, Mesh m) => Limiter s m = Collide  { collideType   :: CollideType
                                                 , deltaMomentum :: Momentum s
                                                 , energyDep     :: Double -- For now
                                                 }
                                      | Boundary { boundaryType :: BoundaryType
                                                 , faceIndex    :: MeshFace m
                                                 }
                                      | Census   { deltaMomentum :: Momentum s
                                                 }
deriving instance (Space s, Show s, Show (Momentum s), 
                   Mesh m, Show m, Show (MeshFace m) ) => Show (Limiter s m)

-- | An Event is motion, and a limiter.
data (Space s, Mesh m) => Event s m = Event { eventMotion :: (Motion s), eventLimiter :: (Limiter s m) }
deriving instance (Space s, Mesh m, Show (Limiter s m), Show (Motion s)) => Show (Event s m)

-- | Strip the motion and just test the limiter.
isFinalEvent :: (Mesh m, Space s) => Event s m -> Bool
isFinalEvent = isFinalLimiter . eventLimiter

-- | Returns True if a step limiter stops the particle streaming.
isFinalLimiter :: (Mesh m, Space s) => Limiter s m -> Bool
isFinalLimiter (Collide Absorb _ _) = True
isFinalLimiter (Boundary Escape _)  = True
isFinalLimiter (Census _)           = True
isFinalLimiter _                    = False

