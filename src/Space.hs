{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Data types for Three-dimensonal space in Cartesian coordinates

module Space(Distance (..),
             Position (..),
             Momentum (..),
             Motion (..), displacement, move,
             Direction (), dir, direction, direction_unsafe,
             translate) where

import Data.Vector.V3
import Data.Vector.Class
import Approx
import NumUnit

-- * Data types

-- | Distance, scalar
newtype Distance  = Distance { dis :: Double  } deriving (Eq, Ord, Show, Num)

-- | Position, a 3D vector
newtype Position = Position { pos :: Vector3 } deriving (Show, Eq)

-- | Momentum, a 3D vector
newtype Momentum = Momentum { momentum :: Vector3 } deriving (Show, Eq)

-- | Motion, a 3D vector
newtype Motion = Motion { motion :: Vector3 } deriving (Show, Eq)

-- | Direction, a 3D vector of magnitude 1.
newtype Direction = Direction { dir :: Vector3 } deriving (Show, Eq)


-- * Construction and manupulation of space quantities

-- | Normalizes the vector when creating a Direction
direction :: Vector3 -> Direction
direction v = Direction (vnormalise v)

-- | Assumes that the vector is already normalized
direction_unsafe :: Vector3 -> Direction
direction_unsafe v = Direction v

-- | Defines normalize operation for Directions
instance NumUnit Direction where
  normalize (Direction v) = direction v

-- | Create motion from a distance and direction
displacement :: Direction -> Distance -> Motion
displacement (Direction dir) (Distance dist) = Motion (dist *| dir)

-- | Applies Motion to a Position to find a new Position
move :: Position -> Motion -> Position
move (Position p) (Motion m) = Position (p + m)

-- | Translates object along the direction vector
translate :: Position -- ^ Initial position
             -> Direction -- ^ Direction vector
             -> Distance  -- ^ Movement distance
             -> Position  -- ^ New position
translate (Position x) (Direction v) (Distance d) = Position $ x + (d *| v)



instance Approx Position where
  within_eps epsilon (Position a) (Position b)  = within_eps epsilon a b
  
instance Approx Direction where
  within_eps epsilon (Direction a) (Direction b) = within_eps epsilon a b
  
instance Approx Distance where
  within_eps epsilon (Distance a) (Distance b) = within_eps epsilon a b
