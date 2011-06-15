{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module NormalizedPhantom where

import Control.Applicative
import Control.Monad

import Data.Vector.Class
import Data.Vector.V2  -- For examples below.

data Gen
data Norm

-- Phantom type which carries type information for normalized
-- status. This differs from the example in
-- http://www.haskell.org/haskellwiki/Phantom_type by having an extra
-- type parameter for the underlying data type.
--
-- Don't export the constructor when used in other modules
data NV a v = NV { val :: v } deriving (Show, Eq)

-- Aliases for the normalized and general values
type Normalized v = NV Norm v 
type General    v = NV Gen v 

-- | Function to make NV values, as we hide the NV constructor.
general :: v -> NV Gen v
general = NV

instance Monad (NV Gen) where
  return = NV
  (>>=) (NV v) f = f v

instance Applicative (NV Gen) where
  pure = return
  (<*>) = ap

instance Functor (NV Gen) where
  fmap = (<$->)
  
-- | A weak application operator which removes the normalized status.
(<$->) :: (v -> b) -> NV a v -> NV Gen b
(<$->) f = pure . f . val

-- | Class Mag defines magnitude and normalization operations. 
class Mag a v where
  magnitude  :: NV a v -> Double
  magnitude2 :: NV a v -> Double
  magnitude2 = (^2) . magnitude
  normalize  :: NV a v -> NV Norm v

-- | An instance for *all* normalized values.
instance Mag Norm v where
    magnitude  = const 1.0
    magnitude2 = const 1.0
    normalize  = id

-- | An instance for vectors. This will cause problems if I add other
-- contexts.
instance Vector v => Mag Gen v where
    magnitude  = vmag . val
    magnitude2 (NV v) = vdot v v
    normalize  = NV . vnormalise . val
    
instance Mag Gen Double where
    magnitude  = abs . val
    magnitude2 = (^2) . val
    normalize (NV v) = NV $ if v > 0 then 1.0 else (negate 1.0)

-- Examples

vecFunc :: Vector2 -> Vector2
vecFunc (Vector2 x y) = Vector2 (x+y) (x-y)

vecFunc2 :: Vector2 -> Double -> Vector2
vecFunc2 (Vector2 x y) d = Vector2 (x+d) (x-d)

v  = general $ Vector2 0.5 (negate 1.0)
m  = magnitude v
vn = normalize v

v'  = vecFunc <$> v
vn' = vecFunc <$-> vn

v'' = vecFunc2 <$> v <*> (pure 2.0)

-- I can derive from Mag Gen
newtype R = R Double deriving (Mag Gen)

-- I can also derive from Mag Norm. This is a problem, so hide Norm type.
newtype Rn = Rn Double deriving (Mag Norm)

-- vn = normalize v


