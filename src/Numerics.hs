module Numerics where
{-- A module for various numeric definitions useful in building domain-specific numeric types --}

-- | A newtype for aribtrary values in (0,1)
newtype UnitInterval n = UnitInterval n deriving (Show)

