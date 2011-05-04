{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Mesh.Spherical where

import Data.Functor
import Data.Function
import Data.Ix
import Data.Sequence as Seq

import Mesh.Classes
import SpaceTime.Classes
import SpaceTime.Spherical1D
import Numerics
import Approx
import RandomSamples

data SphericalMeshCell = SphericalMeshCell { getIndex :: Int }
                       deriving (Eq, Ord, Ix, Show)

data SphericalDirection = Inward | Outward
                        deriving (Eq, Ord, Show)

data SphericalMesh = SphericalMesh { radii :: Seq Radius
                                   , bc :: BoundaryCondition }
                   deriving Show

inward_neighbor :: SphericalMesh
                   -> SphericalMeshCell
                   -> Neighbor SphericalMeshCell
inward_neighbor mesh cell
  | getIndex cell == 0 = Cell cell -- ^ Crossing origin.
  | otherwise = Cell cell{ getIndex = getIndex cell - 1 }


outward_neighbor :: SphericalMesh
                    -> SphericalMeshCell
                    -> Neighbor SphericalMeshCell
outward_neighbor mesh cell
  | getIndex cell == size mesh = Void
  | otherwise = Cell cell{ getIndex = getIndex cell + 1 }

cell_min :: SphericalMesh -> SphericalMeshCell -> Radius
cell_min mesh cell
    | getIndex cell == 0 = Radius 0
    | otherwise = (radii mesh) `Seq.index` (getIndex cell - 1)

cell_max :: SphericalMesh -> SphericalMeshCell -> Radius
cell_max mesh cell = (radii mesh) `Seq.index` ( getIndex cell )

-- !!!: Depends on the answer to my other question. Why is Void even needed? My naive
-- assumption would be that normal coordinates aren't Void, and that you just need it
-- as a special case sometimes. Then I'd use SphericalMeshCell to be an Int-triple, and
-- Maybe SphericalCell when you need to care about the special case.

-- ANS: I answered this in Cartesian3D.hs

-- BTW, the index handling in general is suspicious to me. If I see something like
--
-- > foo `index` getIndex cell
--
-- like you have above, then I have to wonder why you don't look up via the cell in the
-- first place.

-- ANS: I wanted to capture in one place the relationship between
-- cells and the correct entries in the radius data. In this mesh, we
-- have a collection of radii in increasing order, with each cell
-- being bounded above and below by consecutive radii. (Except cell 0,
-- it's lower radius is implicitly zero, so I don't store this
-- information) Functions cell_min and cell_max are where I encode the
-- fact that the upper radius for cell #i is in the position i of
-- radii and the lower radius is in position i-1.

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
  size = Seq.length . radii

  -- | Search the mesh for the cell containing the given location.
  cell_find mesh location =
    let rads  = radii mesh
        pairs = Seq.zip (Radius 0 <| rads) (rads)
    in SphericalMeshCell
       <$> Seq.findIndexL (cell_bounds_test (==) location) pairs

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
