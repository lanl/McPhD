{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Normalized where

import Data.Vector.V2

class Mag v where
    type NormType v :: * --  NormType v = Normalized v usually. Would make a nice default.
    normalize :: v -> NormType v
    magnitude :: v -> Double
    
class Mag v => Norm v where    
    type DeNormType v :: *
    extract :: v -> DeNormType v

-- Newtype wrapper for normalized values
newtype (Mag v) => Normalized v = Normalized { norm_value :: v }

-- Making Normalized an instance of Mag:  
instance Mag v => Mag (Normalized v) where
  type NormType (Normalized v) = Normalized v  -- ^ It is it's own normalized type
  normalize = id
  magnitude = const 1.0

-- Making Normalized an instance of Norm gives us functions for converting back
instance Mag v => Norm (Normalized v) where
    type DeNormType (Normalized v) = v
    extract = norm_value
    


instance Mag Double where
  type NormType Double = Normalized Double
  normalize d = Normalized $ d/abs d
  magnitude = abs
  
instance Mag Int where                 
  type NormType Int = Normalized Int
  normalize d
      | d < 0     = Normalized (negate 1)
      | otherwise = Normalized 1
  magnitude = fromIntegral . abs

