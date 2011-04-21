{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Mesh.Spherical where

import Data.Functor
import Data.List

import Mesh.Classes
import SpaceTime.Classes
import SpaceTime.Spherical1D
import Numerics
import RandomSamples ()
       
data SphericalMeshCell = SphericalMeshCell { index :: Int }  | Void
data SphericalDirection = Inward | Outward

data SphericalMesh = SphericalMesh { radii :: [Radius] }

inward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
inward_cell mesh cell 
    | index cell == 1 = SphericalMeshCell { index = 1 }
    | otherwise       = SphericalMeshCell { index = index cell - 1 }

outward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
outward_cell mesh cell
    | index cell == size mesh = Void
    | otherwise               = SphericalMeshCell { index = index cell + 1}


instance SpaceMesh SphericalMesh where
  type MeshCell  SphericalMesh = SphericalMeshCell
  type MeshFace  SphericalMesh = SphericalDirection
  type MeshSpace SphericalMesh = Spherical1D
  size = length . radii
  
  -- This is too simple; we should use direction to break equality
  -- with a radius. Also, it looks like line noise.
  cell_find mesh location = SphericalMeshCell <$> ( fst <$> ( find ( ( > position location) . snd) (zip [1..] (radii mesh))))
  

  cell_neighbor  mesh cell Inward = inward_cell mesh cell
  cell_neighbor  mesh cell Outward = outward_cell mesh cell
  cell_neighbors mesh cell = [(Inward, inward_cell mesh cell), (Outward, outward_cell mesh cell)]
  cell_boundary = undefined
  
  is_in = undefined
  is_approx_in = undefined
  uniform_sample = undefined
  
      
  -- uniform_sample mesh cell rand =
  --   let (r, rand') = sample_unit_sphere rand
  --       rindex = index cell
  --       rmin = get_radius $ (radii mesh) !! (rindex-1)
  --       rmax = get_radius $ (radii mesh) !! rindex
  --       position = Radius $ rmin + (rmax-rmin)*r
  --       (direction, rand'') = sampleNormalVector2 