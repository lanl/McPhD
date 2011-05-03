{-# LANGUAGE TypeFamilies #-}
module Mesh.Cartesian3D where

import Data.Vector.V3
import Data.Ix
import Data.Sequence


import SpaceTime.Cartesian
import Mesh.Classes

data Cartesian3DCell = Void
                     | Cartesian3DCell { cm_index :: (Int,Int,Int) }
                     deriving (Eq, Show, Ord)

-- TODO: Could you document why a cell can be "Void"?

-- ANS: Void is there to capture the notion of "there is no next cell"
-- when dealing with cells that are on the boundary, for example when
-- querying for neighbors. In other codes, we reserved cell # 0 for
-- this, but I wanted a more clear distinction.
--
-- Maybe could work for this special case, but I'll need to encode
-- more information in the type. For example, the next cell may not be
-- there because it's the edge of the simulation, it's on another
-- processor, etc. I'd add constructors to Cartesian3DCell to handle
-- these.

-- I think I've got a better way to do this using Neighbor
-- below. Should be in here soon.

-- | A datatype representing the possible neighbors of a cell. 
data Neighbor = Cell { neighbor_cell :: Cartesian3DCell } 
              | Edge -- ^ Edge of the simulation
              | Reflection -- ^ Reflecting boundary condition. Particle will not escape.

-- | We make the cell type into an index by prepending void.
instance Ix Cartesian3DCell where
  range (Void             , Void             ) = [Void]
  range (Void             , Cartesian3DCell b) = Void : map Cartesian3DCell (range ((0,0,0), b))
  range (_                , Void             ) = []
  range (Cartesian3DCell a, Cartesian3DCell b) = map Cartesian3DCell (range (a,b))

  inRange (a, b) x = x `elem` range (a, b)

-- TODO: I have added the first line, please verify. range is typically an "inclusive" function.
-- I've added a very inefficient version of inRange, to make the definition complete. A more
-- efficient version should be added. Why exactly do we need the Ix instance?
  
-- ANS: I anticipate storing physical properties defined on the mesh
-- as arrays. Perhaps this is premature? I could easily store these as
-- a map keyed on Cartesian3DCell.

data Cartesian3DDirection = Negative_X
                          | Positive_X
                          | Negative_Y
                          | Positive_Y
                          | Negative_Z
                          | Positive_Z deriving (Eq, Show)

data C3D = Dimensions !Int !Int !Int deriving Show

-- TODO: I've added strictness annotations for good measure. It seems inconsistent to have
-- (Int, Int, Int) to identify a cell above, and then three Int fields here. I'd prefer the
-- latter everywhere.

data Cartesian3DMesh = Cartesian3DMesh {
  c3Ddimensions :: C3D
  , x_coords :: Seq Double
  , y_coords :: Seq Double
  , z_coords :: Seq Double } deriving Show

instance SpaceMesh Cartesian3DMesh where
  type MeshCell Cartesian3DMesh  = Cartesian3DCell
  type MeshFace Cartesian3DMesh  = Cartesian3DDirection
  type MeshSpace Cartesian3DMesh = Cartesian Vector3

  size m = let Dimensions x y z = c3Ddimensions m
           in x*y*z

  cell_find = undefined

  cell_neighbor = undefined

  cell_neighbors = undefined

  cell_boundary = undefined

  is_in_cell = undefined

  is_in_mesh = undefined

  is_approx_in_cell = undefined

  uniform_sample = undefined

  uniform_sample_cell = undefined
