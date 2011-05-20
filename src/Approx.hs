module Approx where

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class

-- | A class which supports approximate equality testing.
infix 4 ~==
infix 4 ~~==
class Approx a where
  within_eps :: Double -> a -> a -> Bool
  (~==) :: a -> a -> Bool  -- ^ Operator for a common tight tolerance
  (~==) = within_eps 1.0e-14

  (~~==) :: a -> a -> Bool  -- ^ Operator for a common looser tolerance
  (~~==) = within_eps 1.0e-8

-- TODO: Perhaps define global constants in some file, and use the constants
-- here rather than the hardcoded values?

instance Approx Double where
  within_eps epsilon a b = abs (a-b) < epsilon

within_eps_Vector :: Vector v => Double -> v -> v -> Bool
within_eps_Vector epsilon a b =
  let d = a - b in vdot d d < epsilon ^ (2 :: Int)

instance Approx Vector1 where within_eps = within_eps_Vector
instance Approx Vector2 where within_eps = within_eps_Vector
instance Approx Vector3 where within_eps = within_eps_Vector
