{-# LANGUAGE TypeFamilies #-}
module SpaceTime.Cart3D where

import SpaceTime.Classes
import Data.Vector.V3
import Data.Vector.Class

data Cart3D = Cart3D { position :: Vector3, direction :: Vector3 } deriving (Eq, Show)

instance Space Cart3D where
  type Distance Cart3D = Double
  stream (Cart3D pos dir) dist = Cart3D (pos + (dir |* dist)) dir

