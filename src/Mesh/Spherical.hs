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

newtype SphericalMeshCell = SphericalMeshCell { getIndex :: Int }
    deriving (Eq, Ord, Ix, Show)

data SphericalDirection = Inward | Outward deriving (Eq, Ord, Show)

data SphericalMesh = SphericalMesh
                     {
                       radii :: Seq Radius
                     , bc    :: BoundaryCondition
                     } deriving Show

cellBound :: SphericalMesh -> SphericalMeshCell -> SphericalDirection -> Radius
cellBound mesh cell Inward
    | getIndex cell == 0 = Radius 0
    | otherwise = (radii mesh) `Seq.index` (getIndex cell - 1)
cellBound mesh cell Outward = (radii mesh) `Seq.index` ( getIndex cell )


-- BTW, the index handling in general is suspicious to me. If I see something like
--
-- > foo `index` getIndex cell
--
-- like you have above, then I have to wonder why you don't look up via the cell in the
-- first place.

-- ???: I don't understand the question. Do you mean why dont I use a
-- map instead, with SphericalMeshCell as the key? Or Drop
-- SphericalMeshCell as a seperate type and just use Int?

outer_cell :: SphericalMesh -> SphericalMeshCell
outer_cell mesh = SphericalMeshCell $ size mesh -1

outer_radius :: SphericalMesh -> Radius
outer_radius mesh = cellBound mesh (outer_cell mesh) Outward

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
                -> SphericalMesh -> SphericalMeshCell -> Spherical1D
                -> Bool
in_cell_test comp mesh cell location =
  let rmin    = cellBound mesh cell Inward
      rmax    = cellBound mesh cell Outward
  in cell_bounds_test comp location (rmin, rmax)



-- | Make SphericalMesh an instance of Mesh
instance Mesh SphericalMesh where
  type MeshCell  SphericalMesh = SphericalMeshCell
  type MeshFace  SphericalMesh = SphericalDirection
  type MeshSpace SphericalMesh = Spherical1D

  -- | # cells = # stored radii
  size = Seq.length . radii

  -- | Search the mesh for the cell containing the given location.
  cell_find mesh location =
    let rads  = radii mesh
        pairs = Seq.zip (Radius 0 <| rads) (rads)
    in SphericalMeshCell
       <$> Seq.findIndexL (cell_bounds_test (==) location) pairs

  cell_neighbor mesh cell Inward
      | getIndex cell == 0 = Cell cell -- ^ Crossing origin.
      | otherwise = Cell cell{ getIndex = getIndex cell - 1 }


  cell_neighbor mesh cell Outward
      | getIndex cell == size mesh = Void
      | otherwise = Cell cell{ getIndex = getIndex cell + 1 }

  cell_neighbors mesh cell = [(Inward,  cell_neighbor mesh cell Inward),
                              (Outward, cell_neighbor mesh cell Outward) ]

  is_in_mesh mesh location = (position location) < (outer_radius mesh)

  is_in_cell        = in_cell_test (==)
  is_approx_in_cell = in_cell_test (~==)


  cell_boundary = undefined



  uniform_sample mesh rand =
    let (radius, rand') = sample_ball1D (outer_radius mesh) rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (Spherical1D radius direction, rand'')

  uniform_sample_cell mesh cell rand =
    let rmin = cellBound mesh cell Inward
        rmax = cellBound mesh cell Outward
        (radius, rand') = sample_annulus1D rmin rmax rand
        (direction, rand'') = sampleNormalVector2 rand'
    in (Spherical1D radius direction, rand'')
