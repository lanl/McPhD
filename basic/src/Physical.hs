{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Physical (
    module Physical
  , module Constants
  , module Numerical
  )
  where

import Data.Vector.Generic.Base as GV
import Data.Vector.Generic.Mutable as GMV
import Data.Vector.Unboxed as V

import Constants
import Numerical

-- | cm
newtype Position     = Position     { pos   :: Vec } deriving (Eq, Show)
-- | dimensionless
newtype Direction    = Direction    { dir   :: Vec } deriving (Eq, Show, Num)
-- | g cm/sec
newtype Momentum     = Momentum     { mom   :: Vec }
  deriving (Eq, Show, Num, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox)
-- | cm/sec
newtype Velocity     = Velocity     { vel   :: Vec } deriving (Eq, Show)
-- | MeV
newtype Energy       = Energy       { e     :: FP  } 
  deriving (Eq, Show, Num, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox)
-- | dimensionless 
newtype EnergyWeight = EnergyWeight { ew    :: FP  } deriving (Eq, Show, Num)
-- | sec
newtype Time         = Time         { t     :: FP  } deriving (Eq, Show, Num)
-- | cm ^2
newtype CrossSection = CrossSection { sigma :: FP  } deriving (Eq, Show, Num)
-- | cm ^ -1
newtype Opacity      = Opacity      { mu    :: FP  } deriving (Eq, Show, Num)
-- | MeV
newtype Temperature  = Temperature  { temp  :: FP  } deriving (Eq, Show)
-- | g/(cc^3)
newtype Density      = Density      { rho   :: FP  } deriving (Eq, Show)
-- | 1/(cc^3)
newtype NDensity     = NDensity     { nrho   :: FP } deriving (Eq, Show) 
-- | dimensionless
newtype NucleonNumber = NucleonNumber {nnucl :: FP } deriving (Eq, Show, Num)
-- | cm
newtype Distance     = Distance     { distance :: FP } deriving (Eq, Show, Num, Ord)
-- | MeV/sec
newtype Luminosity   = Luminosity   { lum :: FP } deriving (Eq,Show,Num,Ord)

-- end of file
