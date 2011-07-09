{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

{-| Datatypes for events generated during simulation.

Events of particle motion fall into categories:

 - Collisions: Consist of various kinds of physical scattering and absoroption

 - Boundaries: Geometric interfaces, e.g. cells, mesh boundaries.

 - Timeout: Particle reached end of the time-step.
-}
module MiniApp.Events where

import qualified Mesh.Classes as Mesh
import Properties

import MiniApp.Physics

-- | Type of Scattering Events
data CollideType = Scatter | Absorb  deriving (Eq, Show, Ord)  -- More kinds to come.
finalCollision :: CollideType -> Bool
finalCollision Scatter = False
finalCollision Absorb  = True

-- | Type of Boundary Events
data BoundaryType = Cell | Escape | Reflect deriving (Eq, Show, Ord)
finalBoundary :: BoundaryType -> Bool
finalBoundary Cell    = False
finalBoundary Escape  = True
finalBoundary Reflect = False


-- | Combining the event types into a single data type with tally information.
data (Mesh.Mesh m) => Event m = Collide  { collideType   :: CollideType
                                         , deltaMomentum :: Momentum (Mesh.MeshSpace m)
                                         , energyDep     :: Energy
                                         }
                              | Boundary { boundaryType  :: BoundaryType
                                         , faceIndex     :: Mesh.MeshFace m
                                         }
                              | Timeout

deriving instance (Mesh.Mesh m, Show m
                  , Show (Mesh.MeshSpace m)
                  , Show (Mesh.MeshFace m)
                  , Show (Momentum (Mesh.MeshSpace m))) => Show (Event m)



-- | Returns True if the event stops the particle streaming.  I don't
-- use a catch-all pattern because I want to be warned if this list is
-- inexhaustive
isFinalEvent :: (Mesh.Mesh m) => Event m -> Bool
isFinalEvent Timeout        = True
isFinalEvent (c@Collide{})  = finalCollision $ collideType c
isFinalEvent (b@Boundary{}) = finalBoundary  $ boundaryType b
