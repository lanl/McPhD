module Particles.Data (
  Point (..),
  Position,
  Direction,
  Distance,
  Limit,
  daxpy) where

    import Data.Vector.V3
    import Data.Vector.Class

    type Point = Vector3

    type Position  = Point
    type Direction = Point
    type Distance  = Double
    type Limit     = Double

    daxpy :: Position -> Direction -> Distance -> Position
    daxpy x y d = vzip (+) x (d *| y) 

