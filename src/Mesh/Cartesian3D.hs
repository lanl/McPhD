{-# LANGUAGE TypeFamilies #-}
module Mesh.Cartesian3D where

import Data.Vector.V3
import Data.Ix
import Data.Sequence

import SpaceTime.Cartesian
import Mesh.Classes

data Cartesian3DCell = Cartesian3DCell Int Int Int
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

-- I think I've got a better way to do this. Should be in here soon.

-- !!!: It's great that you want to avoid using cell 0 for encoding
-- exceptions. Given the type system of Haskell, such approaches are
-- discouraged. It's fine if Maybe isn't enough. You can use Either
-- or use a special type (as you did) instead. I like the new
-- Neighbor-approach much better. See also my comment next to the
-- definition of Neighbor.


-- Why exactly do we need the Ix instance?

-- ANS: I anticipate storing physical properties defined on the mesh
-- as arrays. Perhaps this is premature? I could easily store these as
-- a map keyed on Cartesian3DCell.
--
-- !!!: Are the physical properties of a mesh immutable during a
-- simulation? If yes, an array-like type might be appropriate.

data Cartesian3DDirection = Negative_X
                          | Positive_X
                          | Negative_Y
                          | Positive_Y
                          | Negative_Z
                          | Positive_Z deriving (Eq, Show)

data C3D = Dimensions !Int !Int !Int deriving Show

-- TODO: I've added strictness annotations for good measure. It seems
-- inconsistent to have (Int, Int, Int) to identify a cell above, and
-- then three Int fields here. I'd prefer the latter everywhere.

data Cartesian3DMesh = Cartesian3DMesh {
  c3Ddimensions :: C3D
  , x_coords :: Seq Double
  , y_coords :: Seq Double
  , z_coords :: Seq Double
  , x_low_bc :: BoundaryCondition
  , x_high_bc :: BoundaryCondition
  , y_low_bc  :: BoundaryCondition
  , y_high_bc :: BoundaryCondition
  , z_low_bc  :: BoundaryCondition
  , z_high_bc :: BoundaryCondition } deriving Show

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
