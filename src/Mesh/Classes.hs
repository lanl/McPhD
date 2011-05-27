{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses, StandaloneDeriving #-}

{-| A typeclass for Meshes.

Each mesh is designed for use on a specific space. It defines its own
cell and face indexing schemes. -}

module Mesh.Classes (Mesh (..)
                    , Neighbor (..)
                    , BoundaryCondition (..)
                    , boundary2neighbor) where

import System.Random.Mersenne.Pure64
import Data.Ix

import Numerics ()
import Properties
import Space.Classes

-- | A datatype representing the possible neighbors of a cell.
data Neighbor c = Cell { neighbor_cell :: c }
                | Void -- ^ Edge of the simulation
                | Self -- ^ Cell is it's own neighbor. E.g. reflection
                deriving Show

data BoundaryCondition = Vacuum | Reflection deriving Show

-- | Map boundary conditions on to the correct Neighbor value
boundary2neighbor :: BoundaryCondition -> Neighbor c
boundary2neighbor Vacuum     = Void
boundary2neighbor Reflection = Self

-- | A class for describing operations on meshes.
class (Space (MeshSpace m), Ix (MeshCell m)) => Mesh m where
  type MeshSpace m :: *
  type MeshCell  m :: *
  type MeshFace  m :: *

  -- | Number of cells in the mesh
  size :: m -> Int

  -- | Potentially O(mesh_size) lookup
  cell_find :: m -> MeshSpace m -> Maybe (MeshCell m)

  -- | Neighbor across a given face
  cell_neighbor :: m -> MeshCell m -> MeshFace m -> Neighbor (MeshCell m)

  -- | All neighbors, with faces
  cell_neighbors :: m -> MeshCell m -> [(MeshFace m, Neighbor (MeshCell m))]

  -- | Get the distance to exit a cell, and the face, if lower than
  -- the given distance.
  cell_boundary :: m -> MeshCell m -> MeshSpace m
                   -> Distance
                   -> Maybe (Distance, MeshFace m)

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

