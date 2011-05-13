{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Mesh.Spherical where

import Data.Functor
import Data.Ix
import Data.Sequence as Seq

import Mesh.Classes
import Space.Classes
import Space.Spherical1D
import Numerics
import Approx
import RandomSamples


type SphCell = Int

data SphericalDirection = Inward | Outward deriving (Eq, Ord, Show)

data SphericalMesh = SphericalMesh { radii :: Seq Radius
                                   , bc :: BoundaryCondition }
                   deriving Show

inward_neighbor :: SphericalMesh
                   -> SphCell
                   -> Neighbor SphCell
inward_neighbor _ cell
  | cell == 0 = Cell cell -- ^ Crossing origin.
  | otherwise = Cell $ cell - 1 


outward_neighbor :: SphericalMesh
                    -> SphCell
                    -> Neighbor SphCell
outward_neighbor mesh cell
  | cell == size mesh = Void
  | otherwise         = Cell $ cell + 1

cell_min :: SphericalMesh -> SphCell -> Radius
cell_min mesh cell
    | cell == 0 = Radius 0
    | otherwise = (radii mesh) `Seq.index` (cell - 1)

cell_max :: SphericalMesh -> SphCell -> Radius
cell_max mesh cell = (radii mesh) `Seq.index` cell


outer_cell :: SphericalMesh -> SphCell
outer_cell mesh = size mesh -1

outer_radius :: SphericalMesh -> Radius
outer_radius mesh = cell_max mesh (outer_cell mesh)

-- | Use the position and direction to determine if a location is
-- between two radii.
cell_bounds_test :: (Radius -> Radius -> Bool)
                    -> Spherical1D
                    -> (Radius, Radius)
                    -> Bool
cell_bounds_test comp location (rmin, rmax) =
  let r       = sph1d_position location
      cos_dis = cos_Sph1Ddirection location
  in ( (r > rmin) || ( (r `comp` rmin) && cos_dis >= 0) )  &&
     ( (r < rmax) || ( (r `comp` rmax) && cos_dis < 0) )


-- | Use the position and direction to determine if a location is in a
-- particular cell of the mesh.
in_cell_test :: (Radius -> Radius -> Bool)
                -> SphericalMesh -> SphCell -> Spherical1D
                -> Bool
in_cell_test comp mesh cell location =
  let rmin    = cell_min mesh cell
      rmax    = cell_max mesh cell
  in cell_bounds_test comp location (rmin, rmax)




-- | Make SphericalMesh an instance of Mesh
instance Mesh SphericalMesh where
  type MeshCell  SphericalMesh = SphCell
  type MeshFace  SphericalMesh = SphericalDirection
  type MeshSpace SphericalMesh = Spherical1D

  -- | # cells = # stored radii
  size = Seq.length . radii

  -- | Search the mesh for the cell containing the given location.
  cell_find mesh location =
    let rads  = radii mesh
        pairs = Seq.zip (Radius 0 <| rads) (rads)
    in Seq.findIndexL (cell_bounds_test (==) location) pairs

  cell_neighbor mesh cell Inward  = inward_neighbor mesh cell
  cell_neighbor mesh cell Outward = outward_neighbor mesh cell

  cell_neighbors mesh cell = [(Inward, inward_neighbor mesh cell),
                              (Outward, outward_neighbor mesh cell) ]

  cell_boundary = undefined

  is_in_mesh mesh location = (position location) < (outer_radius mesh)

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
