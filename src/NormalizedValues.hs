{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
module NormalizedValues (Mag(..), Quot(..), deQuot
           , Normalized ()  -- Exporting type but not constructor.
           , unsafe_makeNormal
           , normalized_value
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


-- ANS: I've taken out the Mag instance for Normalized
-- quantities. There wasn't really a use case, as Andres pointed
-- out. Calling normalize on a value which is statically known to be
-- normalized would be a sign something is amiss.


-- | A Num-like class for quantities that need to remain
-- normalized. E.g. certain vectors.  Provides normalize and magnitude
-- functions
class Mag a where
  normalize  :: a -> Normalized a
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude. Often faster.
  magnitude2 x = (magnitude x) ^ (2 ::Integer)
  split      :: a -> Quot a
  split x = Quot (magnitude x) (normalize x)
  join       :: Quot a -> a
  join (Quot s v) = scale s v
  scale      :: Double -> Normalized a -> a
  elacs      :: Normalized a -> Double -> a
  elacs = flip scale

instance Mag Double where
  normalize  d = Normalized $ if d < 0 then -1 else 1  -- Right biased.
  magnitude  d = abs d
  magnitude2 d = d*d
  scale s (Normalized d) = s * d

instance Mag Radius where
  normalize  (Radius r) = Normalized $ Radius $ (normalized_value $ normalize r)
  magnitude  (Radius r) = r
  magnitude2 (Radius r) = r*r
  scale s (Normalized (Radius r)) = Radius (s*r)

-- See notes.org:Problematic instance declarations for thoughts on
-- unifying these instance declarations

instance Mag Vector1 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
  scale s (Normalized v) = s *| v

instance Mag Vector2 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
  scale s (Normalized v) = s *| v

instance Mag Vector3 where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
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
  deriving (Eq, Show, Approx)

unsafe_makeNormal :: (Mag a) => a -> Normalized a
unsafe_makeNormal = Normalized

-- A "quotient field" approach to normalized quantities. The
-- directions are the quotient of the field and the positive reals

data Mag v => Quot v = Quot { quotMag :: Double, quotDir :: Normalized v }

deQuot :: Mag v => Quot v -> v
deQuot (Quot mag dir) = scale mag dir
