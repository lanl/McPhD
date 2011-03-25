{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Mag (Mag
           , Normalized ()  -- Exporting type but not constructor.
           , normalize
           , magnitude
           , magnitude2
           ) where

import Data.Vector.Class
import Data.Vector.V4()
import Data.Vector.V3()
import Data.Vector.V2()
import Data.Vector.V1()

-- | A Num-like class for quantities that need to remain
-- normalized. E.g. certain vectors.  Provides normalize and magnitude
-- functions
class Mag a where
  normalize  :: a -> Normalized a
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude. Used to avoid sqrt in some operations.

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

-- ???: This one requires UndecidableInstances. What am I getting into here?
instance Vector a => Mag a where
  normalize    = Normalized . vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d


-- A data type with hidden constructor to enforce normalization
data (Mag a) => Normalized a = Normalized { normalized_value :: a }


