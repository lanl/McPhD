{-# LANGUAGE FlexibleInstances #-}
module SpaceTime.Test.Cartesian_arbitrary where

import Test.QuickCheck
import Control.Applicative

import Data.Vector.V1
import Data.Vector.V2
import Data.Vector.V3
import SpaceTime.Cartesian

import Test.Vector_arbitrary

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



