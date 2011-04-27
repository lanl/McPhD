{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

{-| A typeclass for Meshes.

Each mesh is designed for use on a specific space. It defines its own
cell and face indexing schemes. -}

module Mesh.Classes (SpaceMesh (..)) where

import System.Random.Mersenne.Pure64

import Numerics ()
import SpaceTime.Classes

-- | A class for describing operations on meshes.
class (Space (MeshSpace m)) => SpaceMesh m where
  type MeshCell  m :: *
  type MeshFace  m :: *
  type MeshSpace m :: *

  -- | Number of cells in the mesh
  size :: m -> Int

  -- | Potentially O(mesh_size) lookup
  cell_find :: m -> MeshSpace m -> Maybe (MeshCell m)

  -- | Neighbor across a given face
  cell_neighbor :: m -> MeshCell m -> MeshFace m -> MeshCell m

  -- | All neighbors, with faces
  cell_neighbors :: m -> MeshCell m -> [(MeshFace m, MeshCell m)]

  -- | Get the distance to exit a cell, and the face.
  cell_boundary :: m -> MeshCell m -> MeshSpace m
                   -> (Distance (MeshSpace m), MeshFace m)

  -- | Is the location in the given cell of the mesh.
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

-- TODO: Are all these functions independent of each other, or would you
-- want to give default definitions for a few?
--
-- The use of Distance in cell_boundary also suggests that you want to
-- require that the MeshSpace of a SpaceMesh (hah!) is actually a Space,
-- so I've added a superclass constraint.

-- ANS: I may need to reconsider some of my names :) But yes, that
-- constraint is correct. There are a few dependencies between methods
-- to write defaults with. Have to think a bit on implementing them.
