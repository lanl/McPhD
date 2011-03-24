{-# LANGUAGE FlexibleInstances #-}
module SpaceTime.Test.Space_arbitrary where

import Test.QuickCheck
import Control.Applicative

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import SpaceTime.Cartesian
import SpaceTime.Spherical1D

import Test.Vector_arbitrary


{- TODO: Arbitrary direction vectors must be unit vectors! -}

{-- I can make particular Cartesian instances into Arbitrary instances
--}
instance Arbitrary (Cartesian Vector1) where  
    arbitrary = Cartesian <$> arbitrary <*> arbitrary
    
instance Arbitrary (Cartesian Vector2) where  
    arbitrary = Cartesian <$> arbitrary <*> arbitrary

instance Arbitrary (Cartesian Vector3) where  
    arbitrary = Cartesian <$> arbitrary <*> arbitrary

{-- ???: How would I make all instances of Cartesian into Arbitrary instances? --}
-- instance (Vector v) => Arbitrary (Cartesian v) where
--     arbitrary = Cartesian <$> arbitrary <*> arbitrary

instance Arbitrary Spherical1D where
  arbitrary = Spherical1D <$> arbitrary <*> arbitrary

