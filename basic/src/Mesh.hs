-- Mesh.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Mesh (Mesh(..)  -- Mesh type now defined in MeshBase
            , bdyEvent
            , distToBdy
            , samplePosition
            , sampleDirection
            , simpleMesh
            , module Cell)
    where

import Physical
import Event
import Cell 
import qualified Sphere1D as Sphere1D
import qualified Cart1D as Cart1D
import qualified Cart3D as Cart3D
import MeshBase 

-- * geometric properties
-- | Given a mesh,cell, and face, determine the Event (returned as Event ctor)
bdyEvent :: Mesh -> CellIdx -> Face -> (FP->Face->Event)
bdyEvent msh cell face = bdyTypeToEvent $ bdyType msh cell face

-- * sampling on meshes
-- | sample a position in the mesh: dispatch to appropriate module
samplePosition :: Mesh -> FP -> FP -> FP -> (Position,CellIdx)
samplePosition msh@(Sphere1D {}) = Sphere1D.sampPos msh  
samplePosition msh@(Cart1D {})   = Cart1D.sampPos   msh  
samplePosition msh@(Cart3D {})   = Cart3D.sampPos   msh  

-- | sample a direction: dispatch to appropriate module
sampleDirection :: Mesh -> FP -> FP -> FP -> Direction
sampleDirection Sphere1D {} = Sphere1D.sampDir  
sampleDirection Cart1D {}   = Cart1D.sampDir    
sampleDirection Cart3D {}   = Cart3D.sampDir   

-- | dispatch on mesh type to appropriate distance function
distToBdy :: Mesh -> CellIdx -> Position -> Direction -> (FP, Face)
distToBdy msh cell psn drn = 
    case msh of 
      Sphere1D {} -> Sphere1D.distToBdy r omega rhi rlo
          where r     = v1x (pos psn)
                omega = v1x (dir drn)
                rhi   = xcomp (pos $ high_b ((mesh msh) ! cell))
                rlo   = xcomp (pos $ low_b  ((mesh msh) ! cell))
      Cart3D {} -> Cart3D.distToBdy x o bl bh
      Cart1D {} -> Cart1D.distToBdy x o bl bh
      where x  = psn
            o  = drn
            bl = high_b ((mesh msh) ! cell)
            bh = low_b  ((mesh msh) ! cell)

-- | look up type of boundary from mesh 
bdyType :: Mesh -> CellIdx -> Face -> BoundaryCondition
bdyType msh cell XLow  = xbc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell XHigh = xbc $ high_bc ((mesh msh) ! cell)
bdyType msh cell YLow  = ybc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell YHigh = ybc $ high_bc ((mesh msh) ! cell)
bdyType msh cell ZLow  = zbc $ low_bc  ((mesh msh) ! cell)
bdyType msh cell ZHigh = zbc $ high_bc ((mesh msh) ! cell)

-- | Given a boundary condition, return the corresponding event ctor
bdyTypeToEvent :: BoundaryCondition -> (FP->Face->Event)
bdyTypeToEvent Vac = Escape
bdyTypeToEvent Refl = Reflect
bdyTypeToEvent Transp = Transmit
bdyTypeToEvent None = error "Cannot associate null boundary with an event"

-- instance, handy for testing. 
simpleMesh :: Mesh
simpleMesh = Sphere1D $ listArray (1,2)
             [(CellProps (Position 0.0) (Position 1.0) (bc1D Vac) (bc1D Transp)),
              (CellProps (Position 1.0) (Position 2.0) (bc1D Transp) (bc1D Vac))]

                                     
-- version
-- $Id$

-- End of file
