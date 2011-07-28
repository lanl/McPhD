{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}


module Approx where

import NumericClasses
import NormalizedValues
import Data.Function

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import Data.Vector.Class

strictTol :: Double
strictTol = 1e-14

relaxedTol :: Double
relaxedTol = 1e-8


-- TODO: Perhaps define global constants in some file, and use the
-- constants here rather than the hardcoded values?

-- | A class which supports approximate equality testing.
infix 4 ~==
infix 4 ~~==
infix 4 ~==~
infix 4 ~~==~
class Approx a where
    within_eps     :: Double -> a -> a -> Bool
    within_eps_rel :: Double -> a -> a -> Bool

    (~==) :: a -> a -> Bool  -- ^ Operator for a common tight tolerance
    (~==) = within_eps strictTol

    (~~==) :: a -> a -> Bool  -- ^ Operator for a common looser tolerance
    (~~==) = within_eps relaxedTol
  
    (~==~) :: a -> a -> Bool
    (~==~) = within_eps_rel strictTol
  
    (~~==~) :: a -> a -> Bool
    (~~==~) = within_eps_rel relaxedTol

-- | General definitions of within_eps and within_eps_rel. Instead of
-- the Approx typeclass, I could use these definitions directly. 

within_eps_impl :: Ord t => (a->a->a) -> (a->t) -> t -> a -> a -> Bool
within_eps_impl diff mag tol val ref = mag (val `diff` ref) < tol

within_eps_rel_impl :: (Ord t, Num t) => (a->a->a) -> (a->t) -> t -> a -> a -> Bool
within_eps_rel_impl diff mag tol val ref
    | mag val == 0   = mag ref < tol
    | mag ref == 0   = mag val < tol
    | otherwise      = within_eps_impl diff mag (tol * mag ref) val ref

instance (Diff a, Mag a) => Approx a where
    within_eps     = within_eps_impl     diff magnitude
    within_eps_rel = within_eps_rel_impl diff magnitude

instance (Normable a, Approx a, Num a) => Approx (Normalized a) where
    within_eps     epsilon = (within_eps epsilon) `on` getValue
    within_eps_rel epsilon = (within_eps_rel epsilon) `on` getValue

within_eps_Vector :: Vector v => Scalar -> v -> v -> Bool
within_eps_Vector tol a b = within_eps_impl (-) (\v -> vdot v v) (tol*tol) a b
                   
within_eps_rel_Vector :: Vector v => Double -> v -> v -> Bool
within_eps_rel_Vector tol a b = within_eps_rel_impl (-) (\v -> vdot v v) (tol*tol) a b

instance Approx Vector1 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector
instance Approx Vector2 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector
instance Approx Vector3 where within_eps     = within_eps_Vector
                              within_eps_rel = within_eps_rel_Vector


within_eps_num :: (Real n) => n -> n -> n -> Bool
within_eps_num = within_eps_impl (-) abs 


