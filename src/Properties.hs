{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| A module containing scalar properties of space
-}
module Properties where

import Test.QuickCheck

import NumericClasses
import Approx


-- * Material Properties

-- | 1/cm
newtype Opacity      = Opacity { opValue :: Double }
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | cm^2
newtype CrossSection = CrossSection { sigma :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | MeV
newtype Energy       = Energy { engValue :: Double }
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | dimensionless
newtype EnergyWeight = EnergyWeight { engwValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | MeV
newtype Temperature  = Temperature { tempValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | g/cc^d
newtype Density      = Density { denValue  :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | 1/cc^d
newtype NDensity     = NDensity { ndenValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- * Space-time properties

-- | cm/second
newtype Speed        = Speed { spValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | cm
newtype Distance     = Distance { distValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)

-- | second
newtype Time         = Time { timeValue :: Double } 
                     deriving (Eq, Show, Num, Ord, Approx, Arbitrary, Mag)




-- * Conversion functions for various properties.

goingAt :: Distance -> Speed -> Time
goingAt (Distance d) (Speed s) = Time (d/s)

gettingTo :: Time -> Speed -> Distance
gettingTo (Time t) (Speed s) = Distance (s*t)

applyWeight :: EnergyWeight -> Energy -> Energy
applyWeight (EnergyWeight wt) (Energy eng) = Energy $ wt*eng