{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Data types for Three-dimensonal space in Cartesian coordinates

module SpaceTime.Space3DCartesian(
  Distance (..)
  , Position (..)
  , Momentum (..)
  , Motion (..), motion
  , Direction (), dir, direction, direction_unsafe, (*->), (+->)
  , Time (..)
  , Speed (..)
  , timeToDistance
  , distanceToTime
  , translate
  , randomDirection
  , randomExponential
  ) where

import Data.Vector.V3
import Data.Vector.Class
import System.Random.Mersenne.Pure64
import Data.Array.IArray
import Test.QuickCheck.Modifiers


import Numerics
import RandomSamples
import Generators
import NormalizedValues
import Approx

-- * Data types

-- | Coordinates
data Coord = X | Y | Z deriving (Show, Eq, Ord, Ix)
-- coords = listArray (X,Z) "xyz" :: Array Coord Char

class VectorType v where
  vector :: v -> Vector3

class ScalarType s where
  scalar :: s -> Double

-- TODO: I have a somewhat bad feeling about these. Is it a good idea to have
-- 3D-specific classes like this?

-- ANS: This was my prototype space definition. Once I get the
-- typeclass spaces and meshes up to speed I should have
-- instance-specific versions for all of these and this one will get
-- retired.

instance VectorType Vector3 where
  vector = id

instance ScalarType Double where
  scalar = id

-- | Position, a 3D vector
newtype Position = Position { pos :: Vector3 } deriving (Eq, Show, Num, Approx, VectorType)

-- | Momentum, a 3D vector
newtype Momentum = Momentum { mom :: Vector3 } deriving (Eq, Show, Num, Approx, VectorType)

-- | Motion, a 3D vector
newtype Motion = Motion { mot :: Vector3 } deriving (Eq, Show, Num, Approx, VectorType)

-- | Distance, scalar
newtype Distance = Distance { dis :: Double  } deriving (Eq, Show, Num, Ord, Approx, ScalarType)

-- | Direction, a 3D vector of magnitude 1.
-- TODO: Replace this with a Normalized Vector3
newtype Direction = Direction { dir :: Vector3 } deriving (Eq, Show, Num, Approx, VectorType)

-- | A scalar representing the magnitude of velocity.
newtype Speed = Speed { speed :: Double } deriving (Eq, Show, Num, Ord, Approx, ScalarType)

-- * Construction and manipulation of space quantities

-- | Normalizes the vector when creating a Direction
direction :: Vector3 -> Direction
direction v = Direction (vnormalise v)

-- | User attests that the vector is already normalised
direction_unsafe :: Vector3 -> Direction
direction_unsafe = Direction

-- | Create motion from a distance and direction
motion :: Direction -> Distance -> Motion
motion (Direction dir) (Distance dist) = Motion (dist *| dir)

infix 7 *->
(*->) :: Direction -> Distance -> Motion
(*->) direction distance = motion direction distance

-- | Applies Motion to a Position to find a new Position
move :: Position -> Motion -> Position
move (Position p) (Motion m) = Position (p + m)

infix 6 +->
(+->) :: Position -> Motion -> Position
(+->) position motion = move position motion

-- | Translates object along the direction vector
translate :: Position  -- ^ Initial position
          -> Direction -- ^ Direction vector
          -> Distance  -- ^ Movement distance
          -> Position  -- ^ New position
translate position direction distance = position +-> (direction *-> distance)


timeToDistance :: Distance -> Speed -> Time
timeToDistance (Distance distance) (Speed speed) = Time (distance / speed)

distanceToTime :: Time -> Speed -> Distance
distanceToTime (Time time) (Speed speed) = Distance (time * speed)



-- | Compute a random Direction from a PureMT
-- TODO: Make a typedef for (a,b) which is a functor.
randomDirection :: PureMT -> (Direction, PureMT)
randomDirection g = let
  (a, g')   = sampleVar g
  (b, g'')  = sampleVar g'
  v = generateNormalVector3 a b
  in (direction $ normalized_value v , g'')

-- | Sample an exponential Distance from a PureMT
randomExponential :: Double -> PureMT -> (Distance, PureMT)
randomExponential lambda g = let
  (a, g') = randomDouble g
  in (Distance $ generateExponential (Positive lambda) (UnitInterval a), g')
