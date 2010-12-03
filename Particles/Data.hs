module Particles.Data (
  Point (..),
  Position,
  Direction,
  Distance,
  Limit,
  daxpy) where


data Point = Point { x::Double, y::Double, z::Double } deriving Show

type Position  = Point
type Direction = Point
type Distance  = Double
type Limit     = Double

daxpy :: Point -> Point -> Double -> Point
daxpy point direction distance = Point
                                 (x point + distance * x direction)
                                 (y point + distance * y direction)
                                 (z point + distance * z direction)
