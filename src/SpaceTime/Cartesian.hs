{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module SpaceTime.Cartesian where

import Approx
import NormalizedValues
import SpaceTime.Classes
import Data.Vector.Class

{-- The cartesian spaces for 1,2 and 3 dimensions are all very
similar, differning only in the dimensionality of the vector used to
store the position and direction information. The streaming operator
can be expressed in terms of the scalar multiplication and vector
addition operatons of this vector.
--}

-- A data type for Cartesian spaces over a vector type
data Cartesian v = Cartesian { cart_position  :: v,
                               cart_direction :: Normalized v }
                 deriving (Eq, Show)

-- When the vector type is Vector, we can define a common streaming operator
instance (Vector v) => Space (Cartesian v) where
    type Distance  (Cartesian v) = Double
    type Position  (Cartesian v) = v
    type Direction (Cartesian v) = Normalized v
    stream (Cartesian pos dir) dist =
      Cartesian (pos + (normalized_value  dir) |* dist) dir
    position  = cart_position
    direction = cart_direction

instance Approx v => Approx (Cartesian v) where
  within_eps epsilon (Cartesian x1 d1) (Cartesian x2 d2) =
    (within_eps epsilon x1 x2) && (within_eps epsilon d1 d2)
