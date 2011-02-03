{-# LANGUAGE TypeFamilies #-}

{-| An exploratory typeclass for Meshes.
-}
module Mesh.Classes (SpaceMesh (..)) where

import Space3DCartesian

-- | A class for describing operations on meshes. Still multi-parameter though.
class SpaceMesh m where
  type MeshCell m     :: *
  type MeshFace m     :: *
  size            :: m -> Integer  
  cell_find       :: m -> Position -> Direction -> MeshCell m -- ^ Potentially O(mesh_size) lookup
  cell_volume     :: m -> MeshCell m -> Double
  cell_neighbor   :: m -> MeshCell m -> MeshFace m -> MeshCell m      -- ^ Neighbor across a given face
  cell_neighbors  :: m -> MeshCell m -> [(MeshFace m, MeshCell m)]    -- ^ All neighbors, with faces
  -- | Get the distance to exit a cell, the face, and the next cell
  cell_exit       :: m -> MeshCell m -> Position -> MeshFace m -> (Distance, MeshFace m, MeshCell m)

-- | Todo: How to do random sampling of positions in cell? 
--     Assume uniform? Include Direction in result?
