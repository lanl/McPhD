{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

module NormalizedValues (Normable(..)
                        , Normalized ()  -- Exporting type but not constructor.
                        , unsafe_makeNormal
                        , getValue
                        , normalVector1
                        , generateNormalVector1
                        , normalVector2
                        , generateNormalVector2
                        , normalVector3
                        , generateNormalVector3
                        ) where

import NumericClasses

import Vectors
import Numerics

import Data.Vector.V3
import Data.Vector.V2
import Data.Vector.V1


-- A data type with hidden constructor to enforce normalization
newtype Normalized a = Normalized { getValue :: a } deriving (Eq, Show)

-- Normable is close to being a Hilbert Space over |R.  I think all
-- it's missing is for v to be an ableian group with a dot product
-- operator, plus the vector space rules. 
class (Mag a, Scale a) => Normable a where
    normalize  :: a -> Normalized a
    normalize v = Normalized $ scale v (1.0 / magnitude v)


-- Normalized quantities have a magnitude, and it's always one. Note
-- that we can't make Normalized an instance of Normable without also
-- making it a member of Scale.
instance Mag (Normalized a) where
  magnitude  = const 1.0
  magnitude2 = const 1.0

instance Normable Double where
  normalize d = Normalized $ if d < 0 then -1 else 1  -- Right biased.

-- The default is good enough.
deriving instance Normable Radius


-- See notes.org:Problematic instance declarations for thoughts on
-- unifying these instance declarations

-- The default instances are good enough.
instance Normable Vector1 where
instance Normable Vector2 where
instance Normable Vector3 where


-- Since we're hiding the Normalized constructor, we need to provide
-- ways of creating various normalized data types.
  
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


unsafe_makeNormal :: (Mag a) => a -> Normalized a
unsafe_makeNormal = Normalized


