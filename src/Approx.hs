module Approx where

import Data.Vector.V3
import Data.Vector.Class

-- | A class which supports approximate equality testing.
infix 4 ~==
infix 4 ~~==
class Approx a where
  within_eps :: Double -> a -> a -> Bool
  (~==) :: a -> a -> Bool
  (~==) = within_eps 1.0e-14 -- | Operator for a common tight tolerance
  
  (~~==) :: a -> a -> Bool
  (~~==) = within_eps 1.0e-8 -- | Operator for a common looser tolerance
  

instance Approx Double where
  within_eps epsilon a b = abs (a-b) < epsilon

instance Approx Vector3 where
  within_eps epsilon a b = let d = a-b in 
    vdot d d < epsilon^(2::Int) 



