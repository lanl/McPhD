module NumUnit where

-- | A Num-like class for quantities that need to remain normalized. E.g. vectors.
-- Provides operations which automatically normalize the result.
class NumUnit a where
  normalize :: a -> a

