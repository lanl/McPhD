{-# OPTIONS_GHC  -XTypeFamilies #-}

{-| An exploratory typeclass for Meshes.
-}
module Mesh.Classes (MeshFamily (..)) where

import Space

-- | FIXME: does the processing of different meshes have enough in common to warrant a typeclass
-- that would provide some useful abstraction?
class MeshFamily m where
    data Space m     :: *
    data Partition m :: *
    data Data m      :: *
    

-- | A simpler class for describing operations on meshes. Still multi-parameter though.
class SpaceMesh m where
  type Cell m     :: *
  type Face m     :: *
  size            :: m -> Integer  
  cell_find       :: m -> Position -> Direction -> Cell m -- ^ Potentially O(mesh_size) lookup
  cell_volume     :: m -> Cell m -> Double
  cell_neighbor   :: m -> Cell m -> Face m -> Cell m      -- ^ Neighbor across a given face
  cell_neighbors  :: m -> Cell m -> [(Face m, Cell m)]    -- ^ All neighbors, with faces
  -- | Get the distance to exit a cell, the face, and the next cell
  cell_exit       :: m -> Cell m -> Position -> Face m -> (Distance, Face m, Cell m)

-- | Todo: How to do random sampling of positions in cell? 
--     Assume uniform? Include Direction in result?