{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, StandaloneDeriving #-}

{-| A typeclass for Meshes.

Each mesh is designed for use on a specific space. It defines its own
cell and face indexing schemes. -}

module Mesh.Classes (Mesh (..)
                    , BoundaryCondition (..)
                    , Crossing (..)
                    , Neighbor (..)
                    , NeighborT
                    , boundaryToCrossing
                    ) where

import System.Random.Mersenne.Pure64
import Data.Ix

import Numerics ()
import Properties
import Space.Classes

-- | A data type for kinds of crossings between cells.
-- These will be used in events which go into the tally.
data Crossing = Face      -- ^ Entered another cell.
              | Boundary  -- ^ Crossed the boundary of the mesh
              | Self      -- ^ Remained in same cell. (E.g. reflection)
                deriving (Show, Eq, Ord)
data BoundaryCondition = Vacuum | Reflection deriving (Show, Eq, Ord)

-- | Look up the correct crossing kind for a boundary condition.
boundaryToCrossing :: BoundaryCondition -> Crossing
boundaryToCrossing Vacuum     = Boundary
boundaryToCrossing Reflection = Self

-- | A type capturing information about a face crossing.
data Neighbor c f = Neighbor
                    { cell :: c
                    , face :: f
                    , crossing :: Crossing
                    } deriving (Show, Eq)

type NeighborT m = Neighbor (MeshCell m) (MeshFace m)


-- | A class for describing operations on meshes.
class (Space (MeshSpace m), Ix (MeshCell m)) => Mesh m where
  type MeshSpace m :: *
  type MeshCell  m :: *
  type MeshFace  m :: *

  -- | Number of cells in the mesh
  size :: m -> Int

  -- | Range of cell indices
  cellRange :: m -> (MeshCell m, MeshCell m)

  -- | Null cell value.
  cellNull :: m -> MeshCell m

  -- | Is the given cell index the null cell?
  isNull :: m -> MeshCell m -> Bool
  isNull mesh cell = (cellNull mesh == cell)

  -- | Potentially O(mesh_size) lookup
  cell_find :: m -> MeshSpace m -> Maybe (MeshCell m)

  -- | Neighbor across a given face
  cell_neighbor :: m -> MeshCell m -> MeshFace m -> NeighborT m

  -- | All neighbors, indexed by face.
  cell_neighbors :: m -> MeshCell m -> [NeighborT m]

  -- | Get the distance to exit a cell, and the face, if lower than
  -- the given distance. Otherwize nothing.
  cell_boundary :: m -> MeshCell m -> MeshSpace m -> (Distance, NeighborT m)

  -- | Is the location in the given cell of the mesh?
  is_in_cell :: m -> MeshCell m -> MeshSpace m -> Bool

  -- | Is the location contained in this mesh?
  is_in_mesh :: m -> MeshSpace m -> Bool

  -- | Allow position to be within epsilon of in, as long as direction
  -- is pointing inward.
  is_approx_in_cell :: m -> MeshCell m -> MeshSpace m -> Bool

  -- | Sample a location uniformly thoughout the mesh
  uniform_sample :: m -> PureMT -> (MeshSpace m, PureMT)

  -- | Sample a location unformly in the given cell.
  uniform_sample_cell :: m -> MeshCell m -> PureMT -> (MeshSpace m, PureMT)
