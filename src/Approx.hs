{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Approx where

import Numerics
import NormalizedValues

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class

strictTol :: Double
strictTol = 1e-14

relaxedTol :: Double
relaxedTol = 1e-8


-- | A class which supports approximate equality testing.
infix 4 ~==
infix 4 ~~==
class Approx a where
  within_eps :: Double -> a -> a -> Bool
  within_eps_rel :: Double -> a -> a -> Bool
  
  (~==) :: a -> a -> Bool  -- ^ Operator for a common tight tolerance
  (~==) = within_eps strictTol

  (~~==) :: a -> a -> Bool  -- ^ Operator for a common looser tolerance
  (~~==) = within_eps relaxedTol
  
  (~==~) :: a -> a -> Bool
  (~==~) = within_eps_rel strictTol
  
  (~~==~) :: a -> a -> Bool
  (~~==~) = within_eps_rel relaxedTol

-- TODO: Perhaps define global constants in some file, and use the constants
-- here rather than the hardcoded values?

relativeCompare :: (Num a, Ord a) => a -> a -> a -> Bool
relativeCompare tol val ref
    | val == 0  = abs ref < tol
    | otherwise = abs (val - ref) < tol * abs val


instance Approx Double where
  within_eps epsilon a b = abs (a-b) < epsilon
  within_eps_rel = relativeCompare

within_eps_Vector :: Vector v => Scalar -> v -> v -> Bool
within_eps_Vector epsilon a b =
  let d = a - b in vdot d d < epsilon ^ (2 :: Int)
                   
within_eps_rel_Vector :: Vector v => Double -> v -> v -> Bool
within_eps_rel_Vector epsilon val ref = 
    let d = val - ref in vdot d d < epsilon ^ (2::Int) * vdot val val

instance Approx Vector1 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector
instance Approx Vector2 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector
instance Approx Vector3 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector

deriving instance Approx Radius
deriving instance (Norm a) => Approx (Normalized a)

-- Another way:

within_eps' :: (Num a, Mag a) => Double -> a -> a -> Bool
within_eps' tol val ref = magnitude (val-ref) < tol

softEquiv' :: (Num a, Mag a) => Double -> a -> a -> Bool
softEquiv' tol val ref
    | val == 0   = magnitude ref < tol
    | ref == 0   = magnitude val < tol
    | otherwise  = magnitude (val - ref) < ref * tol
      
