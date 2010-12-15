-- Data types for Three-dimensonal space in Cartesian coordinates

module Space (
  Position (..),
  Direction (..),
  Distance (..),
  translate) where

import Data.Vector.V3
import Data.Vector.Class

-- | Position, a 3d vector
newtype Position  = Position  { pos :: Vector3 } deriving Show

-- | Direction, a 3d vector
newtype Direction = Direction { dir :: Vector3 } deriving Show

-- | Distance, scalar
newtype Distance  = Distance  { val :: Double  } deriving Show

-- | Translates object along the direction vector
translate :: Position -- ^ Initial position
             -> Direction -- ^ Direction vector
             -> Distance -- ^ Movement distance
             -> Position -- ^ New position
translate (Position x) (Direction v) (Distance d) = Position $ vzip (+) x (d *| v)


