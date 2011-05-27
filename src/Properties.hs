{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| A module containing scalar properties of space
-}
module Properties where

import Test.QuickCheck

import Approx
import NormalizedValues

-- * Material Properties
newtype Opacity     = Opacity     { opValue   :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype Energy      = Energy      { engValue  :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype Temperature = Temperature { tempValue :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype Density     = Density     { denValue  :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype NDensity    = NDensity    { ndenValue :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- * Space-time properties
newtype Speed       = Speed       { spValue   :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype Distance    = Distance    { distValue :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)
newtype Time        = Time        { timeValue :: Double } deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

goingAt :: Distance -> Speed -> Time
goingAt (Distance d) (Speed s) = Time (d/s)

gettingTo :: Time -> Speed -> Distance
gettingTo (Time t) (Speed s) = Distance (s*t)


