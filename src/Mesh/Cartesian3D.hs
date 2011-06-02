{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Mesh.Cartesian3D where

import Data.Vector.V3
import Data.Sequence
import Data.Ix

import Space.Cartesian
import Mesh.Classes

data Cartesian3DCell = Cartesian3DCell Int Int Int
                     deriving (Eq, Show, Ord, Ix)

-- !!!: Are the physical properties of a mesh immutable during a
-- simulation? If yes, an array-like type might be appropriate.

-- ANS: Yes. This is related to your discussion with Tim about
-- time-stepping. During one step of particle movement, the material
-- background, and hence these parameters, will be fixed.

data Cartesian3DDirection = Negative_X
                          | Positive_X
                          | Negative_Y
                          | Positive_Y
                          | Negative_Z
                          | Positive_Z deriving (Eq, Show)

data C3D = Dimensions !Int !Int !Int deriving Show

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

instance Mesh Cartesian3DMesh where
  type MeshCell Cartesian3DMesh  = Cartesian3DCell
  type MeshFace Cartesian3DMesh  = Cartesian3DDirection
  type MeshSpace Cartesian3DMesh = Cartesian Vector3

  size m = let Dimensions x y z = c3Ddimensions m in x*y*z

  cellRange = undefined

  cell_find = undefined

  cell_neighbor = undefined

  cell_neighbors = undefined

  cell_boundary = undefined
  
  is_in_cell = undefined

  is_in_mesh = undefined

  is_approx_in_cell = undefined

  uniform_sample = undefined

  uniform_sample_cell = undefined
