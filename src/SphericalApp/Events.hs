{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

{-| Datatypes for events generated during simulation.

Events of particle motion fall into categories:

 - Collisions: Consist of various kinds of physical scattering and absoroption

 - Boundaries: Geometric interfaces, e.g. cells, mesh boundaries.

 - Timeout: Particle reached end of the time-step.
-}
module SphericalApp.Events where

import qualified Space.Classes as Space
import Space.Spherical1D

import qualified Mesh.Classes as Mesh
import Mesh.Spherical

import Properties

-- | Type of Scattering Events
data CollideType = Scatter | Absorb  deriving (Eq, Show, Ord)  -- More kinds to come.
finalCollision :: CollideType -> Bool
finalCollision Scatter = False
finalCollision Absorb  = True

-- | Type of Boundary Events
data BoundaryType = Face | Escape | Reflect deriving (Eq, Show, Ord)
finalBoundary :: BoundaryType -> Bool
finalBoundary Face    = False
finalBoundary Escape  = True
finalBoundary Reflect = False

-- | Combining the event types into a single data type with tally information.
data Event = Collide       { collideType :: CollideType
                           , momentumDep :: Space.Velocity Spherical1D
                           , energyDep   :: Energy
                           }
           | BoundaryCross { boundaryType :: BoundaryType
                           , faceIndex    :: Mesh.MeshFace SphericalMesh
                           }
           | Timeout deriving Show


-- | Returns True if the event stops the particle streaming.  I don't
-- use a catch-all pattern because I want to be warned if this list is
-- inexhaustive
isFinalEvent :: Event -> Bool
isFinalEvent Timeout             = True
isFinalEvent (c@Collide{})       = finalCollision $ collideType c
isFinalEvent (b@BoundaryCross{}) = finalBoundary  $ boundaryType b
