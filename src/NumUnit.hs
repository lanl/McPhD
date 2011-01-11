module NumUnit where

import Data.Vector.Class
import Data.Vector.V4
import Data.Vector.V3
import Data.Vector.V2
import Data.Vector.V1

-- | A Num-like class for quantities that need to remain
-- normalized. E.g. certain vectors.  Provides normalize and magnitude
-- functions
class NumUnit a where
  normalize  :: a -> a
  magnitude  :: a -> Double
  magnitude2 :: a -> Double  -- ^ Square of the magnitude. Used to avoid square-roots.

instance NumUnit Double where
  normalize  d = if d < 0 then -1 else 1 -- ^ Right biased.
  magnitude  d = abs d
  magnitude2 d = d*d


-- TODO: Is there some way to make Data.Vector.Vector a subclass of
-- NumUnit, so I only have to do this once?
  
instance NumUnit Vector4 where
  normalize    = vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d

instance NumUnit Vector3 where
  normalize    = vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
  
instance NumUnit Vector2 where
  normalize    = vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
  
instance NumUnit Vector1 where
  normalize    = vnormalise
  magnitude    = vmag
  magnitude2 d = vdot d d
