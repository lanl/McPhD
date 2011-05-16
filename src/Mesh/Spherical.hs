{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Mesh.Spherical where

import Data.Sequence as Seq
import Data.Vector.Class
import Data.Vector.V2

import Mesh.Classes
import Space.Classes
import Space.Spherical1D
import Numerics
import Approx
import RandomSamples
import NormalizedValues


type SphCell = Int

data SphericalDirection = Inward | Outward deriving (Eq, Ord, Show)

data SphericalMesh = SphericalMesh
                     {
                       radii :: Seq Radius
                     , bc    :: BoundaryCondition
                     } deriving Show



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
    in Seq.findIndexL (cellBoundsTest (==) location) pairs

  cell_neighbor _ cell Inward
      | cell == 0 = Cell cell -- ^ Crossing origin.
      | otherwise = Cell $ cell - 1 
  cell_neighbor mesh cell Outward
      | cell == size mesh = Void
      | otherwise         = Cell $ cell+1

  cell_neighbors mesh cell = [(Inward,  cell_neighbor mesh cell Inward),
                              (Outward, cell_neighbor mesh cell Outward) ]

  is_in_mesh mesh location = (position location) < (outer_radius mesh)

  is_in_cell        = inCellTest (==)
  is_approx_in_cell = inCellTest (~==)



  uniform_sample mesh rand =
    let (Radius radius, rand') = sample_ball1D (outer_radius mesh) rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (radius *| normalized_value direction, rand'')

  uniform_sample_cell mesh cell rand =
    let rmin = cellBound mesh cell Inward
        rmax = cellBound mesh cell Outward
        (Radius radius, rand') = sample_annulus1D rmin rmax rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (radius *| normalized_value direction, rand'')


  cell_boundary = undefined


outer_radius :: SphericalMesh -> Radius
outer_radius mesh = right where ( _ :> right) = viewr $ radii mesh


-- | Return the boundary of a cell in the given direction
cellBound :: SphericalMesh -> SphCell -> SphericalDirection -> Radius
cellBound mesh cell Inward
    | cell == 0 = Radius 0
    | otherwise = (radii mesh) `Seq.index` (cell - 1)
cellBound mesh cell Outward = (radii mesh) `Seq.index` cell


-- | Use the position and direction to determine if a location is
-- between two radii.
cellBoundsTest :: (Radius -> Radius -> Bool)
                    -> Spherical1D
                    -> (Radius, Radius)
                    -> Bool
cellBoundsTest comp location (rmin, rmax) =
  let r       = position location
      cos_dis = (v2x . normalized_value . direction) location
  in ( (r > rmin) || ( (r `comp` rmin) && cos_dis >= 0) )  &&
     ( (r < rmax) || ( (r `comp` rmax) && cos_dis <  0) )


-- | Use the position and direction to determine if a location is in a
-- particular cell of the mesh.
inCellTest :: (Radius -> Radius -> Bool)
                -> SphericalMesh -> SphCell -> Spherical1D
                -> Bool
inCellTest comp mesh cell location =
  let rmin    = cellBound mesh cell Inward
      rmax    = cellBound mesh cell Outward
  in cellBoundsTest comp location (rmin, rmax)


