{-# LANGUAGE TypeFamilies #-}
module SpaceTime.Cart2D where

import SpaceTime.Classes
import Data.Vector.V2
import Data.Vector.Class

data Cart2D = Cart2D

newtype PosT = PosT { pos :: Vector2 }
newtype DirT = DirT { dir :: Vector2 }
newtype DisT = DisT { dis :: Double  }

instance Space Cart2D where
  type Position  Cart2D = PosT
  type Direction Cart2D = DirT
  type Distance  Cart2D = DisT
  stream (Location (PosT pos) (DirT dir)) (DisT dist) =
    Location (PosT $ pos + (dir |* dist)) (DirT dir)
