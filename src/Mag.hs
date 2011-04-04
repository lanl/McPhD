{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Mag (Mag
           , Normalized ()  -- Exporting type but not constructor.
           , normalize
           , magnitude
           , magnitude2
           , normalVector2
           , normalVector3
           ) where

import Vectors

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

{- ???: Can't do this because of duplicate instances. Any way around this? -}
-- instance (RealFloat a) => Mag a where
--   normalize  d = if d < 0 then -1 else 1 -- ^ Right biased.
--   magnitude  d = abs d
--   magnitude2 d = d*d

-- !!!: Generally, you should try to avoid instances of the form
--
-- instance A a => B a where ...
--
-- They're almost always a sign that something isn't modelled suitably for
-- the Haskell class system.
--
-- I can see several options to work around it; not all of them are adequate in
-- every situation. Here are a few:
--
-- (1) If the above definitions are the only ones that you want, then it's better
-- not to define a class Mag at all, but instead just define three functions,
-- such as:
--
-- normalize :: RealFloat a => a -> Normalized a
-- ...
--
-- with the definitions given above. But this does not seem to be the case
-- here.
--
-- (2) You can wrap the types to help the type checker:
--
-- newtype NormFloat  a = NormFloat  a
-- newtype NormVector a = NormVector a
-- instance RealFloat a => Mag (NormFloat a)  where ...
-- instance Vector a    => Mag (NormVector a) where ...
--
-- This will disambiguate the situation for the type checker, and avoid
-- the need for overlapping instances, but it will make the use of the code
-- less pleasant.
--
-- (3) If the classes have a limited number of instances you're interested
-- in, you can specialize the instance declaration to the ground types rather
-- than the global form. This is what you've done for RealFloat/Double, but
-- not for Vector.
--
-- (4) You might consider changing the class hierarchy. You could make Double
-- an instance of the Vector class and then use solution (1). That should work.
-- That being said, turning a Double into a V1 is a single constructor application.
-- You might even be able to do without the instance for Double ...

-- ???: This one requires UndecidableInstances. What am I getting into here?
instance Vector a => Mag a where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

-- !!!: As I said above, such instances are problematic. GHC never considers
-- the part left of the => when picking an instance. So in principle, this
-- declaration says "everything is in Mag, try to find out later that everything's
-- a vector too". In particular, something like
--
-- instance A a => C a
-- instance B a => C a
--
-- will never work. GHC will not look which of A or B hold in order to choose
-- the instance.

{- Functions for making normalized vectors. There are here because I
don't want to expose the Normalized constructor. This really hampers
the extensibility of the Normalized type and Mag class-}

normalVector1 :: Double -> Normalized Vector1
normalVector1 x = let Normalized n = normalize x in Normalized $ Vector1 n

normalVector2 :: Double -> Normalized Vector2
normalVector2 phi = Normalized $ polarToNormalVector2 phi

normalVector3 :: Double -> Double -> Normalized Vector3
normalVector3 phi theta = Normalized $ sphericalToNormalVector3 phi theta


-- A data type with hidden constructor to enforce normalization
data (Eq a, Show a, Mag a) => Normalized a = Normalized { normalized_value :: a } deriving (Eq, Show)
