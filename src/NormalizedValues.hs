{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module NormalizedValues (Mag
           , Normalized ()  -- Exporting type but not constructor.
           , unsafe_makeNormal
           , normalized_value
           , normalize
           , magnitude
           , magnitude2
           , normalVector1
           , generateNormalVector1
           , normalVector2
           , generateNormalVector2
           , normalVector3
           , generateNormalVector3
           ) where

import Vectors
import Numerics
import Approx

import Data.Vector.Class
import Data.Vector.V3
import Data.Vector.V2
import Data.Vector.V1

-- | A Num-like class for quantities that need to remain
-- normalized. E.g. certain vectors.  Provides normalize and magnitude
-- functions
class Mag a where
  normalize  :: a -> Normalized a
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude.

-- This one requires Overlapping instances to avoid ambiguity with instance Vector
instance Mag Double where
  normalize  d = Normalized $ if d < 0 then -1 else 1 -- ^ Right biased.
  magnitude  d = abs d
  magnitude2 d = d*d

instance Mag Radius where
  normalize (Radius r) = Normalized $ Radius $ (normalized_value $ normalize r)
  magnitude (Radius r) = r  
  magnitude2 (Radius r) = r*r
  

{- ???: Can't do this because of duplicate instances. Any way around this? -}
-- instance (RealFloat a) => Mag a where
--   normalize  d = if d < 0 then -1 else 1 -- ^ Right biased.
--   magnitude  d = abs d
--   magnitude2 d = d*d

{-- Removing this instance declaration until I
understand the issues better. -}
-- instance Vector a => Mag a where
--   normalize    = Normalized . vnormalise
--   magnitude    = vmag
--   magnitude2 d = vdot d d

instance Mag Vector1 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

instance Mag Vector2 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

instance Mag Vector3 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
  
-- instance Mag (Normalized Vector1) where    
--   normalize = Normalize . normalized_value
--   magnitude = const (1.0::Double)
--   magnitude2 = const (1.0::Double)
  
-- instance Mag (Normalized Vector2) where
--   normalize = id
--   magnitude = const (1.0::Double)
--   magnitude2 = const (1.0::Double)
               
-- instance Mag (Normalized Vector3) where
--   normalize = id
--   magnitude = const (1.0::Double)
--   magnitude2 = const (1.0::Double)



{- Functions for making normalized vectors. There are here because I
don't want to expose the Normalized constructor. This really hampers
the extensibility of the Normalized type and Mag class-}

normalVector1 :: Double -> Normalized Vector1
normalVector1 x = let Normalized n = normalize x in Normalized $ Vector1 n

generateNormalVector1 :: Var -> Normalized Vector1
generateNormalVector1 (UnitInterval x) = normalize $ Vector1 (x-0.5)

normalVector2 :: AzimuthAngle -> Normalized Vector2
normalVector2 = Normalized . polarToNormalCartesian

generateNormalVector2 :: Var -> Normalized Vector2
generateNormalVector2 = Normalized . polarToNormalCartesian . sampleAzimuthAngle

normalVector3 :: AzimuthAngle -> ZenithAngle -> Normalized Vector3
normalVector3 phi theta = Normalized $ sphericalToNormalCartesian phi theta

generateNormalVector3 :: Var -> Var -> Normalized Vector3
generateNormalVector3 x y = Normalized $
                          sphericalToNormalCartesian
                          (sampleAzimuthAngle x)
                          (sampleZenithAngle y)


-- A data type with hidden constructor to enforce normalization
newtype Normalized a = Normalized { normalized_value :: a } deriving (Eq, Show)

unsafe_makeNormal :: (Mag a) => a -> Normalized a
unsafe_makeNormal = Normalized

instance (Mag a, Approx a) => Approx (Normalized a) where
  within_eps epsilon (Normalized a) (Normalized b) =
    within_eps epsilon a b
    
  