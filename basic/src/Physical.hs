{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, TemplateHaskell,
  MultiParamTypeClasses,TypeFamilies #-}
module Physical (
  module Physical
  , module Constants
  , module Numerical
  )
  where

import Control.DeepSeq
-- import Data.Vector.Generic.Base as GV
-- import Data.Vector.Generic.Mutable as GMV
-- import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving as V
import Data.Serialize
import Constants
import Numerical

import Text.Printf -- for PrintfArg instances

-- | cm
newtype Position = Position { pos :: Vec }
  deriving (Eq,Show,Read,Num,Fractional)
derivingUnbox "Position" [t| Position -> Vec |] [| pos |] [| Position |]

-- | dimensionless
newtype Direction = Direction { dir :: Vec }
  deriving (Eq,Show,Read,Num)
derivingUnbox "Direction" [t| Direction -> Vec |] [| dir |] [| Direction |]

-- | g cm/sec
newtype Momentum = Momentum { mom :: Vec }
  deriving (Eq,Show,Read,Num,Serialize,NFData)
derivingUnbox "Momentum" [t| Momentum -> Vec |] [| mom |] [| Momentum |]

-- | cm/sec
newtype Velocity = Velocity { vel :: Vec }
  deriving (Eq,Show,Read)
derivingUnbox "Velocity" [t| Velocity -> Vec |] [| vel |] [| Velocity |]

-- | MeV
newtype Energy = Energy { e :: FP }
  deriving (Eq, Show, Read, Ord, Num, Fractional, Floating, NFData,Serialize,
    PrintfArg)
derivingUnbox "Energy" [t| Energy -> Vec |] [| e |] [| Energy |]

-- | dimensionless
newtype EnergyWeight = EnergyWeight { ew :: FP }
  deriving (Eq, Show, Read, Ord, Num, Fractional, Floating, NFData,Serialize,
    PrintfArg)
derivingUnbox "EnergyWeight"
  [t| EnergyWeight -> Vec |] [| ew |] [| EnergyWeight |]

-- | sec
newtype Time = Time { t :: FP } deriving (Eq,Show,Read,Num)
derivingUnbox "Time" [t| Time -> Vec |] [| t |] [| Time |]

-- | cm ^2
newtype CrossSection = CrossSection { sigma :: FP } deriving (Eq,Show,Read,Num)
derivingUnbox "CrossSection"
  [t| CrossSection -> Vec |] [| sigma |] [| CrossSection |]

-- | cm ^ -1
newtype Opacity = Opacity { mu :: FP } deriving (Eq,Show,Read,Num)
derivingUnbox "Opacity" [t| Opacity -> Vec |] [| mu |] [| Opacity |]

-- | MeV
newtype Temperature = Temperature { temp :: FP } deriving (Eq,Show,Read)
derivingUnbox "Temperature"
  [t| Temperature -> Vec |] [| temp |] [| Temperature |]

-- | g/(cc^3)
newtype Density = Density { rho :: FP } deriving (Eq,Show,Read)
derivingUnbox "Density" [t| Density -> Vec |] [| rho |] [| Density |]

-- | 1/(cc^3)
newtype NDensity = NDensity { nrho :: FP } deriving (Eq,Show,Read)
derivingUnbox "NDensity" [t| NDensity -> Vec |] [| nrho |] [| NDensity |]

-- | dimensionless
newtype NucleonNumber = NucleonNumber {nnucl :: FP } deriving (Eq,Show,Read,Num)
derivingUnbox "NucleonNumber"
  [t| NucleonNumber -> Vec |] [| nnucl |] [| NucleonNumber |]

-- | cm
newtype Distance = Distance { distance :: FP } deriving (Eq, Show, Read, Num,
  Ord, NFData,Serialize)
derivingUnbox "Distance" [t| Distance -> Vec |] [| distance |] [| Distance |]

-- | MeV/sec
newtype Luminosity = Luminosity { lum :: FP } deriving (Eq,Show,Read,Num,Ord)
derivingUnbox "Luminosity" [t| Luminosity -> Vec |] [| lum |] [| Luminosity |]

-- | dimensionless
newtype ElectronFraction = ElectronFraction {ef :: FP} deriving (Eq,Show,Read,
  Num,Ord)
derivingUnbox "ElectronFraction"
  [t| ElectronFraction -> Vec |] [| ef |] [| ElectronFraction |]


-- end of file
