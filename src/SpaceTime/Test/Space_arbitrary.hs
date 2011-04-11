{-# LANGUAGE FlexibleInstances #-}
module SpaceTime.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative


import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import SpaceTime.Cartesian
import SpaceTime.Spherical1D

import Test.Numeric_arbitrary ()


{-- ???: How would I make all instances of Cartesian into Arbitrary instances? --}

-- !!!: The following instance calls "arbitrary" on the
-- vectors. Not every vector is automatically an instance
-- of the "Arbitrary" class. So your constraint isn't right:
--
-- instance (Vector v) => Arbitrary (Cartesian v) where
--     arbitrary = Cartesian <$> arbitrary <*> arbitrary
--
-- This one works. There isn't actually a need for the
-- "Vector" constraint, although you might want to put it
-- there ...

{- TODO: Arbitrary direction vectors must be unit vectors! -}

{-- ???: Generating overlapping instances for the three kinds of
 Vectors. Compiler suggests incoherent instances, which I suspect
is not correct. -}
-- instance (Vector v, Arbitrary v) => Arbitrary (Cartesian v) where
--     arbitrary = Cartesian <$> arbitrary <*> arbitrary
    
instance Arbitrary (Cartesian Vector1) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  
instance Arbitrary (Cartesian Vector2) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  
instance Arbitrary (Cartesian Vector3) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  

instance Arbitrary Spherical1D where
  arbitrary = Spherical1D <$> arbitrary <*> arbitrary

