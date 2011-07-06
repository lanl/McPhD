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

-- This is kind of ridiculous. I want different names for the mesh and
-- event contexts, but there's an almost perfect 1-1 correspondence
-- between the two.
--
-- Face means the same thing in both contexts.
--
-- Self might mean more than Reflect. E.g. hitting the origin in
-- Spherical coordaintes and still being in cell 0. Although, this
-- could be considered a kind of reflection.
--
-- There may be more than one kind of Boundary. E.g. physical versus
-- computational domain. The latter would probably just be tallied as
-- a face crossing.
convertMeshCross :: Mesh.Crossing -> BoundaryType
convertMeshCross Mesh.Face     = Face
convertMeshCross Mesh.Boundary = Escape
convertMeshCross Mesh.Self     = Reflect

convertNeighbor :: Mesh.NeighborT SphericalMesh -> Event
convertNeighbor (Mesh.Neighbor _ face crossing) =
  BoundaryCross (convertMeshCross crossing) face

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
