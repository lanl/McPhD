{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Physical (
 module Physical
 , module Constants
 , module Numerical
 )
 where

import Control.DeepSeq
import Data.Vector.Unboxed.Deriving

import Constants
import Numerical

-- | cm
newtype Position = Position { pos :: Vec } deriving (Eq, Show,Num, Fractional)
-- | dimensionless
newtype Direction = Direction { dir :: Vec } deriving (Eq, Show, Num, NFData)
-- | g cm/sec
newtype Momentum = Momentum { mom :: Double } deriving (Eq, Show, Num, NFData)
derivingUnbox "Momentum" [t| Momentum -> Vec |] [| mom |] [| Momentum |]

-- | cm/sec
newtype Velocity = Velocity { vel :: Vec } deriving (Eq, Show)
-- | MeV
newtype Energy = Energy { e :: FP } deriving (Eq, Show, Num, Ord, NFData)
derivingUnbox "Energy" [t| Energy->Vec |] [| e |] [| Energy |]

-- | dimensionless
newtype EnergyWeight = EnergyWeight { ew :: FP}
  deriving (Eq, Show, Num, Fractional, Floating, NFData)
derivingUnbox "EnergyWeight" [t| EnergyWeight->FP |] [| ew |] [| EnergyWeight |]


-- | sec
newtype Time = Time { t :: FP } deriving (Eq, Show, Num)
-- | cm ^2
newtype CrossSection = CrossSection { sigma :: FP } deriving (Eq, Show, Num)
-- | cm ^ -1
newtype Opacity = Opacity { mu :: FP } deriving (Eq, Show, Num)
-- | MeV
newtype Temperature = Temperature { temp :: FP } deriving (Eq, Show)
-- | g/(cc^3)
newtype Density = Density { rho :: FP } deriving (Eq, Show)
-- | 1/(cc^3)
newtype NDensity = NDensity { nrho :: FP } deriving (Eq, Show)
-- | dimensionless
newtype NucleonNumber = NucleonNumber {nnucl :: FP } deriving (Eq, Show, Num)
-- | cm
newtype Distance = Distance { distance :: FP }
  deriving (Eq, Show, Num, Ord, NFData)
-- | MeV/sec
newtype Luminosity = Luminosity { lum :: FP } deriving (Eq,Show,Num,Ord)
-- | dimensionless
newtype ElectronFraction = ElectronFraction {ef :: FP}
  deriving (Eq,Show,Num,Ord)

-- end of file
