{-# LANGUAGE TypeSynonymInstances #-}

module Test.TestingTools where

import Test.QuickCheck

import System.Random
import PRNG
import Data.Functor

-- | A newtype for aribtrary RealFloat values in (0,1)
newtype Unit n = Unit n deriving (Show)
instance (Arbitrary n, Random n, RealFloat n) => Arbitrary (Unit n) where
  arbitrary = Unit <$> choose (0.0, 1.0)


instance Arbitrary RNG where
  arbitrary = mkRNG <$> arbitrary
