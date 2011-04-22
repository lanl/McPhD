{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Mesh.Spherical where

import Data.Functor
import Data.List

import Mesh.Classes
import SpaceTime.Classes
import SpaceTime.Spherical1D
import Numerics
import Approx
import RandomSamples
       
data SphericalMeshCell = SphericalMeshCell { index :: Int }  | Void deriving (Eq)
data SphericalDirection = Inward | Outward

data SphericalMesh = SphericalMesh { radii :: [Radius] }

inward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
inward_cell mesh Void = SphericalMeshCell { index = size mesh }
inward_cell _ cell 
    | index cell == 0 = SphericalMeshCell { index = 0 } -- | Crossing origin.
    | otherwise       = SphericalMeshCell { index = index cell - 1 }

outward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
outward_cell _ Void = Void
outward_cell mesh cell 
    | index cell == size mesh = Void -- | Leaving the mesh
    | otherwise               = SphericalMeshCell { index = index cell + 1 }

cell_min :: SphericalMesh -> SphericalMeshCell -> Radius
cell_min mesh Void = (radii mesh) !! (size mesh)
cell_min mesh cell
    | index cell == 0 = Radius 0
    | otherwise       = (radii mesh) !! (index cell - 1)
                        
cell_max :: SphericalMesh -> SphericalMeshCell -> Radius
cell_max _ Void = undefined
cell_max mesh cell = (radii mesh) !! ( index cell ) 

outer_cell :: SphericalMesh -> SphericalMeshCell
outer_cell = SphericalMeshCell . size

outer_radius :: SphericalMesh -> Radius
outer_radius mesh = cell_max mesh (outer_cell mesh)

instance SpaceMesh SphericalMesh where
  type MeshCell  SphericalMesh = SphericalMeshCell
  type MeshFace  SphericalMesh = SphericalDirection
  type MeshSpace SphericalMesh = Spherical1D
  size = length . radii
  
  -- This is too simple; we should use direction to break equality
  -- Also, it looks like line noise.
  cell_find mesh location = SphericalMeshCell <$> ( fst <$> ( find ( ( > position location) . snd) (zip [1..] (radii mesh))))
  

  cell_neighbor  mesh cell Inward = inward_cell mesh cell
  cell_neighbor  mesh cell Outward = outward_cell mesh cell
  cell_neighbors mesh cell = [(Inward, inward_cell mesh cell), (Outward, outward_cell mesh cell)]
  cell_boundary = undefined
  
  is_in_mesh mesh location = 
    let r = position location
    in r < outer_radius mesh

  is_in_cell mesh cell location = 
    let r       = position location
        rmin    = cell_min mesh cell
        rmax    = cell_max mesh cell
        cos_dis = cos_Sph1Ddirection location
    in ( (r > rmin) || (r == rmin && cos_dis >= 0) )  && 
       ( (r < rmax) || (r == rmax && cos_dis < 0) )
    
  is_approx_in_cell mesh cell location =
    let r       = position location
        rmin    = cell_min mesh cell
        rmax    = cell_max mesh cell
        cos_dis = cos_Sph1Ddirection location
    in ( (r > rmin) || (r ~== rmin && cos_dis >= 0) ) &&
       ( (r < rmax) || (r ~== rmax && cos_dis < 0) )
  
  uniform_sample mesh rand = 
    let (radius, rand') = sample_ball1D (outer_radius mesh) rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (Spherical1D radius direction, rand'') 
    
  uniform_sample_cell mesh cell rand =
    let rmin = cell_min mesh cell
        rmax = cell_max mesh cell
        (radius, rand') = sample_annulus1D rmin rmax rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (Spherical1D radius direction, rand'')