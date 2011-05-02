{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Mesh.Spherical where

import Data.Functor
import Data.Function
import Data.Sequence as S

import Mesh.Classes
import SpaceTime.Classes
import SpaceTime.Spherical1D
import Numerics
import Approx
import RandomSamples

data SphericalMeshCell = SphericalMeshCell { getIndex :: Int }  | Void deriving (Eq)
data SphericalDirection = Inward | Outward

data SphericalMesh = SphericalMesh { radii :: Seq Radius }

inward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
inward_cell mesh Void = SphericalMeshCell { getIndex = size mesh }
inward_cell _ cell
    | getIndex cell == 0 = SphericalMeshCell { getIndex = 0 } -- | Crossing origin.
    | otherwise = SphericalMeshCell { getIndex = getIndex cell - 1 }

outward_cell :: SphericalMesh -> SphericalMeshCell -> SphericalMeshCell
outward_cell _ Void = Void
outward_cell mesh cell
    | getIndex cell == size mesh = Void -- | Leaving the mesh
    | otherwise = SphericalMeshCell { getIndex = getIndex cell + 1 }

cell_min :: SphericalMesh -> SphericalMeshCell -> Radius
cell_min mesh Void = (radii mesh) `index` (size mesh)
cell_min mesh cell
    | getIndex cell == 0 = Radius 0
    | otherwise = (radii mesh) `index` (getIndex cell - 1)

cell_max :: SphericalMesh -> SphericalMeshCell -> Radius
cell_max _ Void = undefined
cell_max mesh cell = (radii mesh) `index` ( getIndex cell )

outer_cell :: SphericalMesh -> SphericalMeshCell
outer_cell mesh = SphericalMeshCell $ size mesh -1

outer_radius :: SphericalMesh -> Radius
outer_radius mesh = cell_max mesh (outer_cell mesh)


-- | Use the position and direction to determine if a location is
-- between two radii.
cell_bounds_test :: (Radius -> Radius -> Bool)
                    -> Spherical1D
                    -> (Radius, Radius)
                    -> Bool
cell_bounds_test comp location (rmin, rmax) =
  let r = sph1d_position location
      cos_dis = cos_Sph1Ddirection location
  in ( (r > rmin) || ( (r `comp` rmin) && cos_dis >= 0) )  &&
     ( (r < rmax) || ( (r `comp` rmax) && cos_dis < 0) )


-- | Use the position and direction to determine if a location is in a
-- particular cell of the mesh.
in_cell_test :: (Radius -> Radius -> Bool)
                -> SphericalMesh -> SphericalMeshCell -> Spherical1D
                -> Bool
in_cell_test comp mesh cell location =
  let rmin    = cell_min mesh cell
      rmax    = cell_max mesh cell
  in cell_bounds_test comp location (rmin, rmax)


-- | Make SphericalMesh an instance of SpaceMesh
instance SpaceMesh SphericalMesh where
  type MeshCell  SphericalMesh = SphericalMeshCell
  type MeshFace  SphericalMesh = SphericalDirection
  type MeshSpace SphericalMesh = Spherical1D

  -- | # cells = # stored radii
  size = S.length . radii

  -- | Search the mesh for the cell containing the given location.
  cell_find mesh location =
    let rads  = radii mesh
        pairs = S.zip (Radius 0 <| rads) (rads)
    in SphericalMeshCell
       <$> S.findIndexL (cell_bounds_test (==) location) pairs


  cell_neighbor mesh cell Inward  = inward_cell mesh cell
  cell_neighbor mesh cell Outward = outward_cell mesh cell

  cell_neighbors mesh cell = [(Inward, inward_cell mesh cell),
                              (Outward, outward_cell mesh cell) ]

  cell_boundary = undefined

  is_in_mesh mesh location =
    let r = position location
    in r < outer_radius mesh

  is_in_cell = in_cell_test (==)
  is_approx_in_cell = in_cell_test (~==)

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
