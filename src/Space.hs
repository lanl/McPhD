{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Data types for Three-dimensonal space in Cartesian coordinates

module Space where

import Data.Vector.V3
import Data.Vector.Class

-- | Position, a 3d vector
newtype Position = Position  { pos :: Vector3 } deriving (Show, Eq, Num)

-- | Direction, a 3d vector
newtype Direction = Direction { dir :: Vector3 } deriving (Show, Eq)

direction :: Vector3 -> Direction
direction v = Direction (vnormalise v)


-- | Distance, scalar
newtype Distance  = Distance  { val :: Double  } deriving (Eq, Ord, Num, Show)

-- | Translates object along the direction vector
translate :: Position -- ^ Initial position
             -> Direction -- ^ Direction vector
             -> Distance  -- ^ Movement distance
             -> Position  -- ^ New position
translate (Position x) (Direction v) (Distance d) = Position $ x + (d *| v)


-- | A class like Num for values restricted to an appropiate unit value.
class NumUnit a where
  (+/) :: a -> a -> a
  (-/) :: a -> a -> a
  
instance NumUnit Direction where
  (+/) (Direction a) (Direction b) = direction $ a + b
  (-/) (Direction a) (Direction b) = direction $ a - b

-- | A class which supports approximate equality testing.
class Approx a where
  within_eps :: Double -> a -> a -> Bool
  
instance Approx Double where
  within_eps epsilon a b = abs (a-b) < epsilon

instance Approx Vector3 where
  within_eps epsilon a b = let d = a-b in 
    vdot d d < epsilon^ (2::Integer) 

instance Approx Position where
  within_eps epsilon (Position a) (Position b)  = within_eps epsilon a b
  
instance Approx Direction where
  within_eps epsilon (Direction a) (Direction b) = within_eps epsilon a b
  
instance Approx Distance where
  within_eps epsilon (Distance a) (Distance b) = within_eps epsilon a b
