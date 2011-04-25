{-# LANGUAGE TypeFamilies #-}
module Mesh.Cartesian3D where

import Data.Vector.V3


import SpaceTime.Cartesian
import Mesh.Classes

data Cartesian3DCell = Cartesian3DCell { cm_index :: Int }
                     | Void
                     deriving (Eq, Show)

data Cartesian3DDirection = Negative_X
                          | Positive_X
                          | Negative_Y
                          | Positive_Y
                          | Negative_Z
                          | Positive_Z deriving (Eq, Show)

data C3D = Dimensions Int Int Int deriving Show

data Cartesian3DMesh = Cartesian3DMesh {
  c3Ddimensions :: C3D
  , x_coords :: [Double]
  , y_coords :: [Double]
  , z_coords :: [Double] } deriving Show

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
