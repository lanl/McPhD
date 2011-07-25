{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

module NormalizedValues (Mag(..)
                        , Norm(..)
                        , Quot(..)
                        , Scaled(..)
                        , Normalized ()  -- Exporting type but not constructor.
                        , deQuot
                        , unsafe_makeNormal
                        , normalized_value
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

import Data.Vector.Class
import Data.Vector.V3
import Data.Vector.V2
import Data.Vector.V1


class Mag a => Norm a where
  normalize  :: a -> Normalized a

instance Mag a => Mag (Normalized a) where
  magnitude  = const 1.0
  magnitude2 = const 1.0

instance Mag Double where
  magnitude  = abs
  magnitude2 = (^2)
  
instance Norm Double where
  normalize d = Normalized $ if d < 0 then -1 else 1  -- Right biased.
  scale s (Normalized d) = s * d

deriving instance Mag Radius
deriving instance Norm Radius

-- See notes.org:Problematic instance declarations for thoughts on
-- unifying these instance declarations

instance Mag Vector1 where
  magnitude    = vmag
  magnitude2 d = vdot d d
  
instance Norm Vector1 where
  normalize    = Normalized . vnormalise
  scale s (Normalized v) = s *| v

instance Mag Vector2 where
  magnitude    = vmag
  magnitude2 d = vdot d d
  
instance Norm Vector2 where
  normalize    = Normalized . vnormalise
  scale s (Normalized v) = s *| v

instance Mag Vector3 where
  magnitude    = vmag
  magnitude2 d = vdot d d
  
instance Norm Vector3 where
  normalize    = Normalized . vnormalise
  scale s (Normalized v) = s *| v


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
newtype Normalized a = Normalized { normalized_value :: a }
  deriving (Eq, Show)

unsafe_makeNormal :: (Mag a) => a -> Normalized a
unsafe_makeNormal = Normalized

-- A "quotient field" approach to normalized quantities. The
-- directions are the quotient of the field and the positive reals

data Norm v => Quot v = Quot { quotMag :: Double, quotDir :: Normalized v }

deQuot :: Norm v => Quot v -> v
deQuot (Quot mag dir) = scale mag dir

deriving instance (Show v, Norm v) => Show (Quot v)

-- A data type pairing vector types with scalar multiples. This is useful
-- in computing vector-related quantities in models, without knowing
-- too much about the underlying vector type

data Mag v => Scaled v = Scaled { scalar :: Double, vec :: v }
deriving instance (Show v, Mag v) => Show (Scaled v)

