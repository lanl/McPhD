module Space (
  Position,
  Direction,
  Distance,
  translate) where

import Data.Vector.V3
import Data.Vector.Class

newtype Position  = Position  { pos :: Vector3 } deriving Show
newtype Direction = Direction { dir :: Vector3 } deriving Show
newtype Distance  = Distance  { val :: Double  } deriving Show

translate :: Position -> Direction -> Distance -> Position
translate (Position x) (Direction v) (Distance d) = Position $ vzip (+) x (d *| v) 


