{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Mesh.Spherical where

import Data.Sequence as Seq
import Data.Vector.Class
import Data.Vector.V2

import Mesh.Classes
import Space.Classes
import Space.Spherical1D
import Properties
import Numerics
import Approx
import RandomSamples
import NormalizedValues


data SphericalMesh = SphericalMesh
                     {
                       radii :: Seq Radius
                     , bc    :: BoundaryCondition -- ^ Only one boudary on mesh
                     } deriving Show

type SphNeighbor = NeighborT SphericalMesh
type SphCell = Int
data SphDir = Inward | Outward deriving (Eq, Ord, Show)

-- | Expresses the impact of directions on a cell index.
crossingToIndex :: Crossing -> SphCell -> SphDir-> SphCell
crossingToIndex Face     cell Inward  = cell - 1
crossingToIndex Face     cell Outward = cell + 1
crossingToIndex Self     cell _       = cell
crossingToIndex Boundary _ _          = -1

-- | Convert boundary condition information and cell into the index of
-- the neighboring cell.
cellNeighbor :: SphericalMesh -> SphCell -> SphDir -> SphNeighbor

cellNeighbor _ cell Inward =
  let crossing = case cell of
        0 -> Self
        _ -> Face
      nextCell = crossingToIndex crossing cell Inward
  in Neighbor nextCell Inward crossing

cellNeighbor mesh cell Outward =
  let maxCell  = snd $ cellRange mesh
      crossing = if (cell==maxCell)
                 then boundaryToCrossing $ bc mesh
                 else Face
      nextCell = crossingToIndex crossing cell Outward
  in Neighbor nextCell Outward crossing




instance Mesh SphericalMesh where
  type MeshCell  SphericalMesh = SphCell
  type MeshFace  SphericalMesh = SphDir
  type MeshSpace SphericalMesh = Spherical1D

  -- | # cells = # stored radii
  size = Seq.length . radii

  -- | Cells indexed from zero to size-1
  cellRange m = (0, size m-1)

  -- | Search the mesh for the cell containing the given location.
  cell_find mesh location =
    let rads  = radii mesh
        pairs = Seq.zip (Radius 0 <| rads) (rads)
    in Seq.findIndexL (cellBoundsTest (==) location) pairs

  cell_neighbor = cellNeighbor

  cell_neighbors mesh cell = [ cell_neighbor mesh cell Inward
                             , cell_neighbor mesh cell Outward
                             ]

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

  cell_boundary mesh cell (Vector2 r_xi r_eta) =
      let Radius rmin = cellBound mesh cell Inward
          Radius rmax = cellBound mesh cell Outward
          (d, face) = if (r_eta < rmin) && (r_xi < 0)
                      then (distComp rmin, Inward)
                      else (distComp rmax, Outward)
          neighbor = cell_neighbor mesh cell face
      in (Distance d, neighbor)
      where distComp rad =
                (negate r_xi) + sqrt (rad^(2::Integer) - r_eta^(2::Integer))


outer_radius :: SphericalMesh -> Radius
outer_radius mesh = right where ( _ :> right) = viewr $ radii mesh


-- | Return the boundary of a cell in the given direction
cellBound :: SphericalMesh -> SphCell -> SphDir -> Radius
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
