{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Coordinate.Cartesian where

import Data.Vector.Class
import Data.Vector.V3

import Approx
import Properties
import NormalizedValues
import Coordinate.Classes

-- A data type for Cartesian spaces over a vector type
data Cartesian3D  = Cartesian3D { cart_position  :: Vector3
                                , cart_direction :: Normalized Vector3 }
                    deriving (Eq, Show)

instance Coordinate Cartesian3D where
    type Position Cartesian3D  = Vector3
    type Direction Cartesian3D = Normalized Vector3
    type Velocity Cartesian3D  = Vector3
    stream (Cartesian3D pos dir) (Distance d) =
        Cartesian3D (pos + (getValue dir) |* d) dir
    position = cart_position
    direction = cart_direction

instance Approx Cartesian3D where
  within_eps epsilon (Cartesian3D x1 d1) (Cartesian3D x2 d2) =
    (within_eps epsilon x1 x2) && (within_eps epsilon (getValue d1) (getValue d2))
