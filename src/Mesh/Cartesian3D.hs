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
