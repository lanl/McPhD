{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module SpaceTime.Cartesian where

import SpaceTime.Classes
import Data.Vector.Class
import Mag

{-- The cartesian spaces for 1,2 and 3 dimensions are all very
similar, differning only in the dimensionality of the vector used to
store the position and direction information. The streaming operator
can be expressed in terms of the scalar multiplication and vector
addition operatons of this vector.
--}

{-- TODO: Direction should be a unit vector --}

-- A data type for Cartesian spaces over a vector type
data Cartesian v = Cartesian { position :: v, direction :: v } deriving (Eq, Show)

-- When the vector type is Vector, we can define a common streaming operator
instance (Vector v) => Space (Cartesian v) where
    type Distance (Cartesian v) = Double
    stream (Cartesian pos dir) dist = Cartesian (pos + dir |* dist) dir
