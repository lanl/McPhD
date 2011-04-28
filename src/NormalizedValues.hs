{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}
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

-- A default associated type would be a big help here. This is a todo
-- item at
-- http://hackage.haskell.org/trac/ghc/wiki/TypeFunctionsStatus
class Mag a where
  type NormalizedType a
  normalize  :: a -> NormalizedType a
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude.

instance Mag Double where
  type NormalizedType Double = Normalized Double
  normalize  d = Normalized $ if d < 0 then -1 else 1 -- ^ Right biased.
  magnitude  d = abs d
  magnitude2 d = d*d

instance Mag Radius where
  type NormalizedType Radius = Normalized Radius
  normalize  (Radius r) = Normalized $ Radius $ (normalized_value $ normalize r)
  magnitude  (Radius r) = r
  magnitude2 (Radius r) = r*r

-- See notes.org:Problematic instance declarations for thoughts on
-- unifying these instance declarations

instance Mag Vector1 where
  type NormalizedType Vector1 = Normalized Vector1
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

instance Mag Vector2 where
  type NormalizedType Vector2 = Normalized Vector2
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

instance Mag Vector3 where
  type NormalizedType Vector3 = Normalized Vector3
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

{-- This takes care of the problem of defining Mag instances
for Normalized quantities.
--}

instance Mag (Normalized a) where
  type NormalizedType (Normalized a) = Normalized a
  normalize  = id
  magnitude  = const (1.0::Double)
  magnitude2 = const (1.0::Double)

-- TODO: I'm unconvinced. Adding NormalizedType seems to be a bit
-- over the top. All type info is static, so there's no real advantage
-- in trying to normalize already normalized values. If you really
-- need to (I can't see why you would), you can always un-normalize
-- in such a situation.

{- Functions for making normalized vectors. They are here because I
don't want to expose the Normalized constructor. This really hampers
the extensibility of the Normalized type and Mag class-}

-- TODO: What would you prefer here? I see you export unsafe_makeNormal,
-- which is the same as the Normalized constructor.

-- ANS: I think I was hoping for some kind of qualified access to the
-- constructor, so I could selectively allow functions in other
-- modules create Normalized instances. Something like a friend
-- decleration in C++.

-- Since the normalized quantities are turning out to be just vectors,
-- this isn't as big a deal as I thought.
--
-- TODO: You could simulate this via the module system, but I don't think
-- it's worth it. Module M could export T with its constructor C. Then
-- "friends" can just import M, whereas non-friends would import a module P
-- that imports M and re-exports T, but not C.

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
