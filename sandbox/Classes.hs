{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Classes (Point (..),
                Position, 
                Direction, 
                Distance, 
                Limit, 
                InSpace (..),
                Limited (..),
                Streamable (..)) where

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

-- Typeclasses
-- -----------

class InSpace p where
  position  :: p -> Position
  direction :: p -> Direction
  move      :: p -> Distance -> Position
  move particle distance = daxpy (position particle) (direction particle) distance
  
class Limited p where
  value   :: p -> Limit
  advance :: p -> Distance -> (Limit, Distance)

class Streamable p e where
  stream :: p -> Distance -> Maybe (e, p)
  
class AutoStream p e where
  stream :: p -> Maybe (e, p)