{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

{-| Datatypes for events generated during simulation.

Events of particle motion fall into categories:

 - Collisions: Consist of various kinds of physical scattering and absoroption

 - Boundaries: Geometric interfaces, e.g. cells, mesh boundaries.

 - Timeout: Particle reached end of the time-step.
-}
module MiniApp.Events where

import qualified Space.Classes as Space
import Mesh.Classes hiding (Cell)
import Properties

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
data (Mesh m) => Event m = Collide  { collideType   :: CollideType
                                    , deltaMomentum :: Space.Momentum (MeshSpace m)
                                    , energyDep     :: Energy
                                    }
                         | Boundary { boundaryType  :: BoundaryType
                                    , faceIndex     :: MeshFace m
                                    }
                         | Timeout

deriving instance (Mesh m, Show m
                  , Show (MeshSpace m)
                  , Show (MeshFace m)
                  , Show (Space.Momentum (MeshSpace m))) => Show (Event m)



-- | Returns True if the event stops the particle streaming.  I don't
-- use a catch-all pattern because I want to be warned if this list is
-- inexhaustive
isFinalEvent :: (Mesh m) => Event m -> Bool
isFinalEvent Timeout        = True
isFinalEvent (c@Collide{})  = finalCollision $ collideType c
isFinalEvent (b@Boundary{}) = finalBoundary  $ boundaryType b
