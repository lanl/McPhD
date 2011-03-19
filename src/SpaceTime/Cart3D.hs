{-# LANGUAGE TypeFamilies #-}
module SpaceTime.Cart3D where

import SpaceTime.Classes
import Data.Vector.V3
import Data.Vector.Class

data Cart3D = Cart3D

newtype PosT = PosT { pos :: Vector3 }
newtype DirT = DirT { dir :: Vector3 }
newtype DisT = DisT { dis :: Double  }

instance Space Cart3D where
  type Position  Cart3D = PosT
  type Direction Cart3D = DirT
  type Distance  Cart3D = DisT
  stream (Location (PosT pos) (DirT dir)) (DisT dist) =
    Location (PosT $ pos + (dir |* dist)) (DirT dir)

instance SpaceTime Cart3D where
  type Time  Cart3D = Double
  type Speed Cart3D = Double
  timeFromDistance ::