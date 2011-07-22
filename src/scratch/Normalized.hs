{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}


{--

Yet another approach to normalized values. Bored yet?

Here, I split the notion of scalability by doubles and the notion of
having a magnitude. Things which can be normalized have both of these
attributes.

Quantities which _are normalized_ however, have magnitude, but cannot
be scaled.

This makes normalizing a Normalized quantity a type error.

All normalized quantities are assumed to be of the form Normalized v
for some underlying type v. See NormalizedClass for classes of
"Normalized things", of which Normalized v is an instance. 

--}

module Normalized (
                   Mag(..), Scale(..), Normalized(), getValue, Normable(..)
                  ) where

import Data.Vector.Class
import Data.Vector.V2

-- Things with magnitude in |R+
class Mag v where
    magnitude  :: v -> Double
    magnitude2 :: v -> Double
    
-- Things which can be scaled by Doubles. If v is an abelian group,
-- this should be a vector space over |R.
class Scale v where
    scale :: v -> Double -> v


-- Newtype wrapper for normalized values
newtype Normalized v = Normalized { getValue :: v } deriving Show


-- Normable is close to being a Hilbert Space over |R.  If v was also
-- an abelian group over +, I think it would be.
class (Mag v, Scale v) => Normable v where
    normalize :: v -> Normalized v 
    normalize v = Normalized $ scale v (1.0 / magnitude v)
    
    denormalize :: Normalized v -> v
    denormalize (Normalized v) = v

-- Note that we cannot call normalize on Normalized v, because
-- Normalized v is not an instance of Scale.

-- Normalized quantities have magnitude, but are not scalable. This
-- instance isn't necessary if we make 'Normalized n' a part of class
-- NormalizedClass as in NormalizedClass.hs
instance Mag (Normalized n) where
    magnitude  = const 1.0
    magnitude2 = const 1.0


instance Mag Double      where magnitude = abs; magnitude2 = (^2)
instance Scale Double    where scale = (*)
instance Normable Double where
    
  
instance Mag Vector2      where magnitude = vmag; magnitude2 x = vdot x x
instance Scale Vector2    where scale = (|*)
instance Normable Vector2 where 
  
-- An example function over Normalized values.  
f1 :: Normalized n -> n
f1 = getValue