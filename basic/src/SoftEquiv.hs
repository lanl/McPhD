module SoftEquiv where

import Numerical

-- | Compare numbers for equality within some tolerance. The basic
-- case is, if the absolute value of the difference of the value and
-- the reference divided by the value is less than the tolerance,
-- evaluate True. If the value is zero, evaluate whether the reference
-- is less than the tolerance.
{-# SPECIALIZE softEquiv :: FP -> FP -> FP -> Bool #-}
{-# INLINE softEquiv #-}
softEquiv :: (Num a, Ord a) => a -> a -> a -> Bool
softEquiv val ref tol
  | val == 0  = abs ref < tol
  | otherwise = abs (val - ref) < tol * abs val

stdTol :: FP
stdTol = 1e-11

{-# INLINE (~==~) #-}
(~==~) :: FP -> FP -> Bool
(~==~) = softEquiv stdTol

infix 4 ~==~
